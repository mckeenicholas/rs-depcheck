use once_cell::sync::Lazy;
use oxc::ast::ast::IdentifierReference;
use oxc::ast_visit::Visit;
use oxc::parser::Parser;
use oxc::{allocator::Allocator, span::SourceType};
use regex::Regex;
use std::collections::HashSet;
use std::error::Error;

use crate::js::analyze_project::analyze_js;
use crate::js::js_parser::ModuleAnalysis;
use crate::vue::parser::Parser as VueParser;
use crate::vue::parser::{Attribute, Element, Node};

static SCRIPT_TAG_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"<script(?P<attrs>[^>]*)>((?:.|\n)*?)</script>").unwrap());

static LANG_ATTR_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r#"lang\s*=\s*["']([^"']+)["']"#).unwrap());

static TEMPLATE_TAG_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"(?is)(?P<full><template(?P<attrs>[^>]*)>(?P<content>.*)</template>)").unwrap()
});

static PUG_LANG_ATTR_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r#"(?i)lang\s*=\s*["']pug["']"#).unwrap());

fn extract_script_content(source_text: &str) -> (String, SourceType) {
    if let Some(cap) = SCRIPT_TAG_REGEX.captures(source_text) {
        let attributes = cap.get(1).map_or("", |m| m.as_str());
        let content = cap.get(2).map_or("", |m| m.as_str());

        let source_type = if let Some(lang_cap) = LANG_ATTR_REGEX.captures(attributes) {
            let lang_value = lang_cap.get(1).map_or("", |m| m.as_str());
            match lang_value.to_lowercase().as_str() {
                "ts" | "typescript" => SourceType::default()
                    .with_typescript(true)
                    .with_module(true),
                "tsx" => SourceType::default()
                    .with_typescript(true)
                    .with_jsx(true)
                    .with_module(true),
                "jsx" => SourceType::default().with_jsx(true).with_module(true),
                _ => SourceType::default().with_module(true),
            }
        } else {
            SourceType::default().with_module(true)
        };

        (content.trim().to_string(), source_type)
    } else {
        (String::new(), SourceType::default().with_module(true))
    }
}

/// Extracts the outermost <template> tag including the tag itself.
/// Currently returns an error if the template uses lang="pug".
fn extract_template(source_text: &str) -> Result<String, Box<dyn Error>> {
    if let Some(cap) = TEMPLATE_TAG_REGEX.captures(source_text) {
        // Check for pug lang
        if let Some(attrs) = cap.name("attrs") {
            if PUG_LANG_ATTR_REGEX.is_match(attrs.as_str()) {
                return Err("Pug templates are not currently supported".into());
            }
        }

        if let Some(full_template) = cap.name("full") {
            Ok(full_template.as_str().trim().to_string())
        } else {
            Ok(String::new())
        }
    } else {
        // No template tag found, which is a valid grammar state.
        Ok(String::new())
    }
}

pub fn analyze_vue_template(
    allocator: &Allocator,
    file_contents: &str,
) -> Result<ModuleAnalysis, Box<dyn Error>> {
    let (script_content, script_source_type) = extract_script_content(file_contents);

    if script_content.is_empty() {
        return Ok(ModuleAnalysis {
            imports: Vec::new(),
            exports: Vec::new(),
            unused_imports: Vec::new(),
        });
    }

    let mut analysis_results = analyze_js(allocator, script_content, script_source_type)?;

    let template_content = extract_template(file_contents)?;

    if template_content.is_empty() {
        return Ok(analysis_results);
    }

    let mut vue_parser = VueParser::new(&template_content)?;
    let vue_ast = vue_parser.parse()?;

    // Use the analyzer to get identifiers and errors from the template AST
    let analyzer = VueAstAnalyzer::new(allocator, vue_ast, script_source_type);
    let (template_identifiers, template_errors) = analyzer.get_identifiers();

    if !template_errors.is_empty() {
        let error_string = template_errors.join(", ");
        return Err(error_string.into());
    }

    // Convert the list of identifiers used in the template to a HashSet for efficient lookups.
    let template_identifiers_set: HashSet<_> = template_identifiers.into_iter().collect();

    if !template_identifiers_set.is_empty() {
        analysis_results
            .unused_imports
            .retain(|unused| !template_identifiers_set.contains(&unused.import_info.local_name));
    }

    Ok(analysis_results)
}

// This visitor extracts identifiers from a JS expression AST.
#[derive(Default)]
struct JsExprVisitor {
    identifiers: Vec<String>,
}

impl<'a> Visit<'a> for JsExprVisitor {
    fn visit_identifier_reference(&mut self, ident: &IdentifierReference<'a>) {
        self.identifiers.push(ident.name.to_string());
    }
}

struct VueAstVisitor<'a> {
    allocator: &'a Allocator,
    used_identifiers: &'a mut HashSet<String>,
    errors: &'a mut Vec<String>,    // Collect errors here
    script_source_type: SourceType, // Source type from script section
}

impl<'a> VueAstVisitor<'a> {
    fn visit_node(&mut self, node: Node) {
        match node {
            Node::Element(e) => self.visit_element(e),
            Node::Interpolation(expr) => self.visit_interp(&expr),
            Node::Text(_) => {}
        }
    }

    fn visit_element(&mut self, element: Element) {
        // If the tag name is not a native HTML/SVG/Vue element, it's a custom component.
        // We add its name to the list of used identifiers.
        if !NATIVE_ELEMENTS.contains(element.tag_name.to_lowercase().as_str()) {
            println!("found element: {}", element.tag_name);
            self.used_identifiers.insert(element.tag_name.clone());
        }

        for attr in &element.attributes {
            self.process_attr(attr);
        }

        for node in element.children {
            self.visit_node(node);
        }
    }

    fn process_attr(&mut self, attr: &Attribute) {
        let Attribute { name, value } = attr;

        // Handle shorthand attributes like :id="someId"
        if value.is_none() {
            if let Some(ref_name) = name.strip_prefix(':') {
                self.used_identifiers.insert(ref_name.to_string());
            }
        }

        // Handle Vue directives that contain JS expressions
        if name.starts_with("v-") || name.starts_with('@') || name.starts_with(':') {
            if let Some(value_str) = value {
                let identifiers = self.parse_js_expr(value_str);
                self.used_identifiers.extend(identifiers);
            }
        }
    }

    fn visit_interp(&mut self, expr: &str) {
        let identifiers = self.parse_js_expr(expr);
        self.used_identifiers.extend(identifiers);
    }

    fn parse_js_expr(&mut self, expr_str: &str) -> Vec<String> {
        let source_type = self.script_source_type;
        let parser = Parser::new(self.allocator, expr_str, source_type);
        let parsed_expr = parser.parse_expression();

        match parsed_expr {
            Ok(expr) => {
                let mut js_visitor = JsExprVisitor::default();
                js_visitor.visit_expression(&expr);
                js_visitor.identifiers
            }
            Err(err) => {
                let error_list: Vec<String> = err
                    .into_iter()
                    .map(|e| {
                        // Try to include position information if available
                        if let Some(ref labels) = e.labels {
                            if let Some(label) = labels.first() {
                                let start_pos = label.offset() as usize;
                                let lines_before = &expr_str[..start_pos.min(expr_str.len())];
                                let line_num = lines_before.matches('\n').count() + 1;
                                let last_newline =
                                    lines_before.rfind('\n').map_or(0, |pos| pos + 1);
                                let col_num = start_pos - last_newline + 1;
                                format!(
                                    "Expression '{}' at {}:{}: {}",
                                    expr_str, line_num, col_num, e.message
                                )
                            } else {
                                format!("Expression '{}': {}", expr_str, e.message)
                            }
                        } else {
                            format!("Expression '{}': {}", expr_str, e.message)
                        }
                    })
                    .collect();
                self.errors.extend_from_slice(&error_list);

                vec![]
            }
        }
    }
}

struct VueAstAnalyzer<'a> {
    allocator: &'a Allocator,
    ast: Vec<Node>, // Changed from Node to Vec<Node>
    script_source_type: SourceType,
}

impl<'a> VueAstAnalyzer<'a> {
    pub fn new(allocator: &'a Allocator, ast: Vec<Node>, script_source_type: SourceType) -> Self {
        Self {
            allocator,
            ast,
            script_source_type,
        }
    }

    /// Returns a tuple of (used_identifiers, errors)
    pub fn get_identifiers(self) -> (HashSet<String>, Vec<String>) {
        let mut used_identifiers = HashSet::new();
        let mut errors = Vec::new();

        let mut visitor = VueAstVisitor {
            allocator: self.allocator,
            used_identifiers: &mut used_identifiers,
            errors: &mut errors,
            script_source_type: self.script_source_type,
        };

        // Iterate over all root nodes from the parser
        for node in self.ast {
            visitor.visit_node(node);
        }

        (used_identifiers, errors)
    }
}

// A set of all standard HTML, SVG, and built-in Vue component tags.
// Used to differentiate custom components from native elements.
static NATIVE_ELEMENTS: Lazy<HashSet<&'static str>> = Lazy::new(|| {
    [
        // HTML
        "a",
        "abbr",
        "address",
        "area",
        "article",
        "aside",
        "audio",
        "b",
        "base",
        "bdi",
        "bdo",
        "blockquote",
        "body",
        "br",
        "button",
        "canvas",
        "caption",
        "cite",
        "code",
        "col",
        "colgroup",
        "data",
        "datalist",
        "dd",
        "del",
        "details",
        "dfn",
        "dialog",
        "div",
        "dl",
        "dt",
        "em",
        "embed",
        "fieldset",
        "figcaption",
        "figure",
        "footer",
        "form",
        "h1",
        "h2",
        "h3",
        "h4",
        "h5",
        "h6",
        "head",
        "header",
        "hgroup",
        "hr",
        "html",
        "i",
        "iframe",
        "img",
        "input",
        "ins",
        "kbd",
        "label",
        "legend",
        "li",
        "link",
        "main",
        "map",
        "mark",
        "menu",
        "meta",
        "meter",
        "nav",
        "noscript",
        "object",
        "ol",
        "optgroup",
        "option",
        "output",
        "p",
        "param",
        "picture",
        "pre",
        "progress",
        "q",
        "rp",
        "rt",
        "ruby",
        "s",
        "samp",
        "script",
        "section",
        "select",
        "small",
        "source",
        "span",
        "strong",
        "style",
        "sub",
        "summary",
        "sup",
        "table",
        "tbody",
        "td",
        "template",
        "textarea",
        "tfoot",
        "th",
        "thead",
        "time",
        "title",
        "tr",
        "track",
        "u",
        "ul",
        "var",
        "video",
        "wbr",
        // SVG
        "svg",
        "animate",
        "animateMotion",
        "animateTransform",
        "circle",
        "clipPath",
        "defs",
        "desc",
        "ellipse",
        "feBlend",
        "feColorMatrix",
        "feComponentTransfer",
        "feComposite",
        "feConvolveMatrix",
        "feDiffuseLighting",
        "feDisplacementMap",
        "feDistantLight",
        "feDropShadow",
        "feFlood",
        "feFuncA",
        "feFuncB",
        "feFuncG",
        "feFuncR",
        "feGaussianBlur",
        "feImage",
        "feMerge",
        "feMergeNode",
        "feMorphology",
        "feOffset",
        "fePointLight",
        "feSpecularLighting",
        "feSpotLight",
        "feTile",
        "feTurbulence",
        "filter",
        "foreignObject",
        "g",
        "image",
        "line",
        "linearGradient",
        "marker",
        "mask",
        "metadata",
        "mpath",
        "path",
        "pattern",
        "polygon",
        "polyline",
        "radialGradient",
        "rect",
        "stop",
        "switch",
        "symbol",
        "text",
        "textPath",
        "tspan",
        "use",
        "view",
        // Vue specific built-in components
        "component",
        "transition",
        "transition-group",
        "keep-alive",
        "slot",
        "teleport",
        "suspense",
    ]
    .iter()
    .cloned()
    .collect()
});
