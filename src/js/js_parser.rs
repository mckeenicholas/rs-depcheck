use std::{error::Error, fmt::Debug, fs::File, io::Read, path::Path};

use rustc_hash::{FxHashMap, FxHashSet};

use oxc::{
    ast::ast::*,
    ast_visit::Visit,
    parser::{Parser, ParserReturn},
    semantic::ScopeFlags,
    span::Span,
    syntax::module_record::{ExportEntry, ExportExportName, ImportImportName, ModuleRecord},
};

#[derive(Debug)]
pub struct ImportInfo {
    pub module_path: String,
    pub imported_name: String,
    pub local_name: String,
    pub span: Span,
    pub is_type_only: bool,
    pub is_default: bool,
    pub is_namespace: bool,
}

#[derive(Debug)]
pub struct ExportInfo {
    pub exported_name: String,
    pub local_name: Option<String>,
    pub module_path: Option<String>,
    pub span: Span,
    pub is_type_only: bool,
    pub is_default: bool,
    pub is_re_export: bool,
}

#[derive(Debug)]
pub struct UnusedImport {
    pub import_info: ImportInfo,
    pub reason: String,
}

#[derive(Debug)]
pub struct ModuleAnalysis {
    pub imports: Vec<ImportInfo>,
    pub exports: Vec<ExportInfo>,
    pub unused_imports: Vec<UnusedImport>,
}

#[derive(Default)]
pub struct ModuleAnalyzer {
    imports: Vec<ImportInfo>,
    exports: Vec<ExportInfo>,
    // Track which identifiers are actually used in the code
    used_identifiers: FxHashSet<String>,
    // Map from import local names to import info
    import_map: FxHashMap<String, usize>,
}

// TODO: Fix unused deps false positive for TypeScript type imports.
impl<'a> ModuleAnalyzer {
    pub fn analyze_with_module_record(
        &mut self,
        program: &Program<'a>,
        module_record: &ModuleRecord<'a>,
    ) -> ModuleAnalysis {
        // Extract imports from module record
        self.extract_imports_from_module_record(module_record);

        // Extract exports from module record
        self.extract_exports_from_module_record(module_record);

        // Traverse AST to find used identifiers
        self.visit_program(program);

        // Determine unused imports
        let unused_imports = self.find_unused_imports();

        ModuleAnalysis {
            imports: std::mem::take(&mut self.imports),
            exports: std::mem::take(&mut self.exports),
            unused_imports,
        }
    }

    fn extract_imports_from_module_record(&mut self, module_record: &ModuleRecord<'a>) {
        for import_entry in &module_record.import_entries {
            let local_name = import_entry.local_name.name.to_string();
            let import_info = ImportInfo {
                module_path: import_entry.module_request.name.to_string(),
                imported_name: match &import_entry.import_name {
                    ImportImportName::Name(name) => name.name.to_string(),
                    ImportImportName::NamespaceObject => "*".to_string(),
                    ImportImportName::Default(_) => "default".to_string(),
                },
                local_name: local_name.clone(),
                span: import_entry.local_name.span,
                is_type_only: import_entry.is_type,
                is_default: import_entry.import_name.is_default(),
                is_namespace: import_entry.import_name.is_namespace_object(),
            };

            self.import_map.insert(local_name, self.imports.len());
            self.imports.push(import_info);
        }
    }

    fn extract_exports_from_module_record(&mut self, module_record: &ModuleRecord<'a>) {
        // Local exports
        for export_entry in &module_record.local_export_entries {
            self.exports
                .push(self.create_export_info(export_entry, false));
        }

        // Indirect exports (re-exports)
        for export_entry in &module_record.indirect_export_entries {
            self.exports
                .push(self.create_export_info(export_entry, true));
        }

        // Star exports
        for export_entry in &module_record.star_export_entries {
            self.exports
                .push(self.create_export_info(export_entry, true));
        }
    }

    fn create_export_info(&self, export_entry: &ExportEntry<'a>, is_re_export: bool) -> ExportInfo {
        ExportInfo {
            exported_name: match &export_entry.export_name {
                ExportExportName::Name(name) => name.name.to_string(),
                ExportExportName::Default(_) => "default".to_string(),
                ExportExportName::Null => "".to_string(),
            },
            local_name: export_entry.local_name.name().map(|atom| atom.to_string()),
            module_path: export_entry
                .module_request
                .as_ref()
                .map(|req| req.name.to_string()),
            span: export_entry.span,
            is_type_only: export_entry.is_type,
            is_default: export_entry.export_name.is_default(),
            is_re_export,
        }
    }

    fn find_unused_imports(&self) -> Vec<UnusedImport> {
        let mut unused = Vec::new();

        for import_info in &self.imports {
            let is_used = self.used_identifiers.contains(&import_info.local_name);

            if !is_used {
                let reason = if import_info.is_type_only {
                    "Type-only import is never used".to_string()
                } else if import_info.is_namespace {
                    "Namespace import is never used".to_string()
                } else if import_info.is_default {
                    "Default import is never used".to_string()
                } else {
                    format!("Named import '{}' is never used", import_info.imported_name)
                };

                unused.push(UnusedImport {
                    import_info: ImportInfo {
                        module_path: import_info.module_path.clone(),
                        imported_name: import_info.imported_name.clone(),
                        local_name: import_info.local_name.clone(),
                        span: import_info.span,
                        is_type_only: import_info.is_type_only,
                        is_default: import_info.is_default,
                        is_namespace: import_info.is_namespace,
                    },
                    reason,
                });
            }
        }

        unused
    }
}

impl<'a> Visit<'a> for ModuleAnalyzer {
    fn visit_identifier_reference(&mut self, ident: &IdentifierReference<'a>) {
        // Mark this identifier as used
        self.used_identifiers.insert(ident.name.to_string());
    }

    fn visit_binding_identifier(&mut self, _ident: &BindingIdentifier<'a>) {
        // Don't mark binding identifiers as "used" - they're declarations, not usages
        // The actual usage will be tracked via IdentifierReference
    }

    // Add this method to track TypeScript type references
    fn visit_ts_type_reference(&mut self, type_ref: &TSTypeReference<'a>) {
        // Mark the type name as used
        match &type_ref.type_name {
            TSTypeName::IdentifierReference(ident) => {
                self.used_identifiers.insert(ident.name.to_string());
            }
            TSTypeName::QualifiedName(qualified) => {
                // For qualified names like A.B.C, we need to track the root identifier
                self.visit_ts_qualified_name(qualified);
            }
            TSTypeName::ThisExpression(_) => {
                // `this` type annotations don't correspond to imports, so we can ignore them.
            }
        }

        // Continue visiting type arguments if present
        if let Some(type_args) = &type_ref.type_arguments {
            self.visit_ts_type_parameter_instantiation(type_args);
        }
    }

    // Track generic type parameter instantiations (e.g., Array<T>, Promise<User>)
    fn visit_ts_type_parameter_instantiation(
        &mut self,
        type_params: &TSTypeParameterInstantiation<'a>,
    ) {
        for param in &type_params.params {
            self.visit_ts_type(param);
        }
    }

    // Track "as" type assertions/casts
    fn visit_ts_as_expression(&mut self, expr: &TSAsExpression<'a>) {
        // Visit the expression being cast
        self.visit_expression(&expr.expression);
        // Visit the type annotation (this marks the type as used)
        self.visit_ts_type(&expr.type_annotation);
    }

    // Track angle bracket type assertions (<Type>value)
    fn visit_ts_type_assertion(&mut self, expr: &TSTypeAssertion<'a>) {
        // Visit the type annotation (this marks the type as used)
        self.visit_ts_type(&expr.type_annotation);
        // Visit the expression being cast
        self.visit_expression(&expr.expression);
    }

    // Track satisfies expressions (value satisfies Type)
    fn visit_ts_satisfies_expression(&mut self, expr: &TSSatisfiesExpression<'a>) {
        // Visit the expression
        self.visit_expression(&expr.expression);
        // Visit the type annotation (this marks the type as used)
        self.visit_ts_type(&expr.type_annotation);
    }

    // Track instantiation expressions (func<Type>())
    fn visit_ts_instantiation_expression(&mut self, expr: &TSInstantiationExpression<'a>) {
        // Visit the expression being instantiated
        self.visit_expression(&expr.expression);
        // Visit the type arguments (this marks the types as used)
        self.visit_ts_type_parameter_instantiation(&expr.type_arguments);
    }

    // Helper method to handle qualified type names
    fn visit_ts_qualified_name(&mut self, qualified: &TSQualifiedName<'a>) {
        match &qualified.left {
            TSTypeName::IdentifierReference(ident) => {
                self.used_identifiers.insert(ident.name.to_string());
            }
            TSTypeName::QualifiedName(nested_qualified) => {
                self.visit_ts_qualified_name(nested_qualified);
            }
            TSTypeName::ThisExpression(_) => {
                // `this` type annotations don't correspond to imports, so we can ignore them.
            }
        }
    }

    // Also add support for type queries (typeof)
    fn visit_ts_type_query(&mut self, type_query: &TSTypeQuery<'a>) {
        match &type_query.expr_name {
            TSTypeQueryExprName::IdentifierReference(ident) => {
                self.used_identifiers.insert(ident.name.to_string());
            }
            TSTypeQueryExprName::QualifiedName(qualified) => {
                self.visit_ts_qualified_name(qualified);
            }
            TSTypeQueryExprName::TSImportType(import_type) => {
                self.visit_ts_import_type(import_type);
            }
            TSTypeQueryExprName::ThisExpression(_) => {
                // `this` type annotations don't correspond to imports, so we can ignore them.
            }
        }

        if let Some(type_args) = &type_query.type_arguments {
            self.visit_ts_type_parameter_instantiation(type_args);
        }
    }

    // Override call expressions to handle generic function calls like func<Type>()
    fn visit_call_expression(&mut self, expr: &CallExpression<'a>) {
        self.visit_expression(&expr.callee);

        // Check for TypeScript type arguments in function calls
        if let Some(type_args) = &expr.type_arguments {
            self.visit_ts_type_parameter_instantiation(type_args);
        }

        for arg in &expr.arguments {
            self.visit_argument(arg);
        }
    }

    // Override new expressions to handle generic constructor calls like new Class<Type>()
    fn visit_new_expression(&mut self, expr: &NewExpression<'a>) {
        self.visit_expression(&expr.callee);

        // Check for TypeScript type arguments in constructor calls
        if let Some(type_args) = &expr.type_arguments {
            self.visit_ts_type_parameter_instantiation(type_args);
        }

        for arg in &expr.arguments {
            self.visit_argument(arg);
        }
    }

    fn visit_member_expression(&mut self, expr: &MemberExpression<'a>) {
        match expr {
            MemberExpression::StaticMemberExpression(static_expr) => {
                self.visit_expression(&static_expr.object);
                // Don't visit the property as it's not a reference to an identifier
            }
            MemberExpression::ComputedMemberExpression(computed_expr) => {
                self.visit_expression(&computed_expr.object);
                self.visit_expression(&computed_expr.expression);
            }
            MemberExpression::PrivateFieldExpression(private_expr) => {
                self.visit_expression(&private_expr.object);
            }
        }
    }

    // Continue visiting other nodes...
    fn visit_variable_declarator(&mut self, declarator: &VariableDeclarator<'a>) {
        // Visit the initializer if present
        if let Some(init) = &declarator.init {
            self.visit_expression(init);
        }
        // Don't visit the id as it's a binding, not a usage
    }

    fn visit_function(&mut self, func: &Function<'a>, _scope_flags: ScopeFlags) {
        // Visit function body but not parameters (they're bindings)
        if let Some(body) = &func.body {
            self.visit_function_body(body);
        }
    }

    fn visit_assignment_expression(&mut self, expr: &AssignmentExpression<'a>) {
        // Visit the right side (the value being assigned)
        self.visit_expression(&expr.right);

        // For the left side, we need to be careful about what we visit
        // as it might contain both bindings and references
        match &expr.left {
            AssignmentTarget::AssignmentTargetIdentifier(ident) => {
                self.visit_identifier_reference(ident);
            }
            AssignmentTarget::ComputedMemberExpression(computed) => {
                self.visit_expression(&computed.object);
                self.visit_expression(&computed.expression);
            }
            AssignmentTarget::StaticMemberExpression(static_member) => {
                self.visit_expression(&static_member.object);
            }
            AssignmentTarget::PrivateFieldExpression(private) => {
                self.visit_expression(&private.object);
            }
            _ => {}
        }
    }
}

// Helper function to create the analyzer and run analysis
pub fn analyze_module<'a>(
    program: &Program<'a>,
    module_record: &ModuleRecord<'a>,
) -> ModuleAnalysis {
    let mut analyzer = ModuleAnalyzer::default();
    analyzer.analyze_with_module_record(program, module_record)
}

// Example usage function
pub fn print_analysis_results(analysis: &ModuleAnalysis) {
    println!("=== IMPORTS ===");
    for import in &analysis.imports {
        println!(
            "    Import: '{}' as '{}' from '{}' (type: {}, default: {}, namespace: {})",
            import.imported_name,
            import.local_name,
            import.module_path,
            import.is_type_only,
            import.is_default,
            import.is_namespace
        );
    }

    println!("\n=== EXPORTS ===");
    for export in &analysis.exports {
        println!(
            "    Export: '{}' (local: {:?}, from: {:?}, type: {}, default: {}, re-export: {})",
            export.exported_name,
            export.local_name,
            export.module_path,
            export.is_type_only,
            export.is_default,
            export.is_re_export
        );
    }

    println!("\n=== UNUSED IMPORTS ===");
    for unused in &analysis.unused_imports {
        println!(
            "    Unused: '{}' from '{}' - {}",
            unused.import_info.local_name, unused.import_info.module_path, unused.reason
        );
    }
}
