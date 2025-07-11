use clap::Parser;
use std::path::PathBuf;
use std::{
    collections::{HashMap, HashSet},
    error::Error,
    path::Path,
};
use walkdir::{DirEntry, WalkDir};

// Add this import
use oxc::allocator::Allocator;

use parser::parse_and_analyze;

pub mod vue;

pub mod package_json_reader;
pub mod parser;

use package_json_reader::read_package_json;

use crate::{package_json_reader::PackageDependencies, parser::ModuleAnalysis};

const DEFAULT_PACKAGE_JSON_PATH: &str = "package.json";
const TARGET_EXTENSIONS: &[&str] = &["js", "jsx", "ts", "tsx", "vue", "svelte"];
const SKIP_DIRS: &[&str] = &["node_modules", "dist", "build"];

#[derive(Parser)]
struct Args {
    /// Directory to search
    #[arg(short, long, default_value = ".")]
    dir: String,
    /// package.json location
    #[arg(long, default_value = DEFAULT_PACKAGE_JSON_PATH)]
    pkg: String,
}

#[derive(Debug)]
struct DependencyIssues {
    unused_dependencies: Vec<String>,
    missing_dependencies: Vec<String>,
    unused_exports: Vec<(PathBuf, String)>, // (file_path, export_name)
    unused_imports: Vec<(PathBuf, String)>, // (file_path, import_name)
    unused_files: Vec<PathBuf>,
}

// Optimized data structures for faster lookups
#[derive(Debug)]
struct OptimizedDependencyData {
    // Map from (file_path, export_name) to whether it's used
    exports: HashMap<(PathBuf, String), bool>,
    // Map from import_module_path to list of (importing_file, imported_name)
    imports_by_module: HashMap<String, Vec<(PathBuf, String)>>,
    // Set of all imported module paths
    imported_modules: HashSet<String>,
    // Map from file to its resolved canonical path for fast lookups
    file_canonical_paths: HashMap<PathBuf, PathBuf>,
    // Map from canonical_path to original file path
    canonical_to_original: HashMap<PathBuf, PathBuf>,
    // All unused imports from individual file analysis
    unused_imports: Vec<(PathBuf, String)>,
}

fn in_ignore_list(entry: &DirEntry) -> bool {
    entry
        .file_name()
        .to_str()
        .map(|s| (s != "." && s.starts_with(".")) || SKIP_DIRS.contains(&s))
        .unwrap_or(false)
}

fn analyze_project<P: AsRef<Path>>(root_dir: P) -> HashMap<PathBuf, ModuleAnalysis> {
    let mut src_file_info = HashMap::new();
    // Create the allocator once
    let mut allocator = Allocator::new();

    let walker = WalkDir::new(root_dir)
        .into_iter()
        .filter_entry(|e| !in_ignore_list(e));
    for file in walker {
        let dir_entry = match file {
            Ok(dir_entry) => dir_entry,
            Err(e) => {
                eprintln!("Error: {}", e);
                continue;
            }
        };

        let path = dir_entry.path();

        if !dir_entry.file_type().is_file() {
            continue;
        }

        let extension = match path.extension() {
            Some(ext) => ext.to_str().unwrap_or(""),
            None => continue,
        };

        if !TARGET_EXTENSIONS.contains(&extension) {
            continue;
        }

        // Pass the allocator to the parsing function
        let results = parse_and_analyze(&allocator, path);
        match results {
            Ok(module_analysis) => {
                src_file_info.insert(path.to_owned(), module_analysis);
            }
            Err(e) => eprintln!("Failed to analyze {:?}: {}", path, e),
        }

        // Reset the allocator to free memory for the next file
        allocator.reset();
    }

    src_file_info
}

fn build_optimized_data(deps: &HashMap<PathBuf, ModuleAnalysis>) -> OptimizedDependencyData {
    let mut exports = HashMap::new();
    let mut imports_by_module = HashMap::new();
    let mut imported_modules = HashSet::new();
    let mut file_canonical_paths = HashMap::new();
    let mut canonical_to_original = HashMap::new();
    let mut unused_imports = Vec::new();

    // Pre-compute canonical paths for all files
    for file_path in deps.keys() {
        if let Ok(canonical) = file_path.canonicalize() {
            file_canonical_paths.insert(file_path.clone(), canonical.clone());
            canonical_to_original.insert(canonical, file_path.clone());
        }
    }

    for (file_path, analysis) in deps {
        // Process exports
        for export in &analysis.exports {
            if !export.is_default && !export.is_re_export {
                exports.insert((file_path.clone(), export.exported_name.clone()), false);
            }
        }

        // Process imports
        for import in &analysis.imports {
            imported_modules.insert(import.module_path.clone());

            // For relative imports, try to resolve to actual files
            if import.module_path.starts_with('.') {
                if let Some(resolved_file) =
                    resolve_import_to_file(&import.module_path, file_path, &file_canonical_paths)
                {
                    // Mark the export as used if it exists
                    let key = (resolved_file, import.imported_name.clone());
                    if exports.contains_key(&key) {
                        exports.insert(key, true);
                    }
                }
            }

            imports_by_module
                .entry(import.module_path.clone())
                .or_insert_with(Vec::new)
                .push((file_path.clone(), import.imported_name.clone()));
        }

        // Collect unused imports
        for unused_import in &analysis.unused_imports {
            unused_imports.push((
                file_path.clone(),
                unused_import.import_info.local_name.clone(),
            ));
        }
    }

    OptimizedDependencyData {
        exports,
        imports_by_module,
        imported_modules,
        file_canonical_paths,
        canonical_to_original,
        unused_imports,
    }
}

fn resolve_import_to_file(
    import_path: &str,
    from_file: &Path,
    file_canonical_paths: &HashMap<PathBuf, PathBuf>,
) -> Option<PathBuf> {
    if !import_path.starts_with('.') {
        return None;
    }

    let from_dir = from_file.parent()?;
    let resolved_import_path = from_dir.join(import_path);

    // Try different extensions
    const EXTENSIONS_TO_TRY: &[&str] = &["", "js", "jsx", "ts", "tsx", "svelte"];

    for ext in EXTENSIONS_TO_TRY {
        let mut path_with_ext = resolved_import_path.as_os_str().to_owned();
        if !ext.is_empty() {
            path_with_ext.push(".");
            path_with_ext.push(ext);
        }

        let candidate_path = PathBuf::from(path_with_ext);
        if let Ok(canonical) = candidate_path.canonicalize() {
            // Find the original path from our file set
            for (original_path, canonical_path) in file_canonical_paths {
                if canonical_path == &canonical {
                    return Some(original_path.clone());
                }
            }
        }
    }

    // Try index files
    for ext in TARGET_EXTENSIONS {
        let index_path = resolved_import_path.join(format!("index.{}", ext));
        if let Ok(canonical) = index_path.canonicalize() {
            for (original_path, canonical_path) in file_canonical_paths {
                if canonical_path == &canonical {
                    return Some(original_path.clone());
                }
            }
        }
    }

    None
}

fn check_dependency_graph(
    deps: HashMap<PathBuf, ModuleAnalysis>,
    package_info: PackageDependencies,
) -> DependencyIssues {
    let optimized_data = build_optimized_data(&deps);

    let mut issues = DependencyIssues {
        unused_dependencies: Vec::new(),
        missing_dependencies: Vec::new(),
        unused_exports: Vec::new(),
        unused_imports: optimized_data.unused_imports,
        unused_files: Vec::new(),
    };

    // Check for unused dependencies - O(n)
    let all_declared_deps: HashSet<String> = package_info
        .dependencies
        .iter()
        .chain(package_info.dev_dependencies.iter())
        .chain(package_info.peer_dependencies.iter())
        .cloned()
        .collect();

    for declared_dep in &all_declared_deps {
        let is_used = optimized_data.imported_modules.iter().any(|imported| {
            imported == declared_dep
                || imported.starts_with(&format!("{}/", declared_dep))
                || imported.starts_with(&format!("@{}/", declared_dep))
        });

        if !is_used {
            issues.unused_dependencies.push(declared_dep.clone());
        }
    }

    // Check for missing dependencies - O(n)
    for imported_module in &optimized_data.imported_modules {
        if imported_module.starts_with('.') {
            continue;
        }

        let package_name = if imported_module.starts_with('@') {
            imported_module
                .split('/')
                .take(2)
                .collect::<Vec<_>>()
                .join("/")
        } else {
            imported_module
                .split('/')
                .next()
                .unwrap_or(imported_module)
                .to_string()
        };

        if !all_declared_deps.contains(&package_name) {
            issues.missing_dependencies.push(package_name);
        }
    }

    issues.missing_dependencies.sort();
    issues.missing_dependencies.dedup();

    // Check for unused exports - O(n) instead of O(n²)
    for ((file_path, export_name), is_used) in &optimized_data.exports {
        if !is_used {
            issues
                .unused_exports
                .push((file_path.clone(), export_name.clone()));
        }
    }

    // Check for unused files
    let entry_files = find_entry_files(&deps);
    let mut used_files = HashSet::new();

    for entry_file in &entry_files {
        mark_file_as_used(
            entry_file,
            &deps,
            &mut used_files,
            &optimized_data.file_canonical_paths,
        );
    }

    for file_path in deps.keys() {
        if !used_files.contains(file_path) {
            issues.unused_files.push(file_path.clone());
        }
    }

    issues
}

fn find_entry_files(deps: &HashMap<PathBuf, ModuleAnalysis>) -> Vec<PathBuf> {
    let mut entry_files = Vec::new();
    const ENTRY_PATTERNS: &[&str] = &["index", "main", "app", "entry"];

    for file_path in deps.keys() {
        let file_stem = file_path.file_stem().and_then(|s| s.to_str()).unwrap_or("");
        if ENTRY_PATTERNS.contains(&file_stem) {
            entry_files.push(file_path.clone());
        }
    }

    if entry_files.is_empty() {
        // If no obvious entry files, use all files as potential entries
        entry_files.extend(deps.keys().cloned());
    }

    entry_files
}

fn mark_file_as_used(
    file_path: &PathBuf,
    deps: &HashMap<PathBuf, ModuleAnalysis>,
    used_files: &mut HashSet<PathBuf>,
    file_canonical_paths: &HashMap<PathBuf, PathBuf>,
) {
    if used_files.contains(file_path) {
        return;
    }

    used_files.insert(file_path.clone());

    if let Some(analysis) = deps.get(file_path) {
        for import in &analysis.imports {
            if let Some(resolved_file) =
                resolve_import_to_file(&import.module_path, file_path, file_canonical_paths)
            {
                mark_file_as_used(&resolved_file, deps, used_files, file_canonical_paths);
            }
        }
    }
}

fn print_dependency_issues(issues: &DependencyIssues) {
    if !issues.unused_dependencies.is_empty() {
        println!("\n=== UNUSED DEPENDENCIES ===");
        for dep in &issues.unused_dependencies {
            println!("  {}", dep);
        }
    }

    if !issues.missing_dependencies.is_empty() {
        println!("\n=== MISSING DEPENDENCIES ===");
        for dep in &issues.missing_dependencies {
            println!("  {}", dep);
        }
    }

    if !issues.unused_exports.is_empty() {
        println!("\n=== UNUSED EXPORTS ===");
        for (file, export) in &issues.unused_exports {
            println!("  {} exports '{}'", file.display(), export);
        }
    }

    if !issues.unused_imports.is_empty() {
        println!("\n=== UNUSED IMPORTS ===");
        for (file, import) in &issues.unused_imports {
            println!("  {} imports '{}'", file.display(), import);
        }
    }

    if !issues.unused_files.is_empty() {
        println!("\n=== UNUSED FILES ===");
        for file in &issues.unused_files {
            println!("  {}", file.display());
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let project_base_dir = Path::new(&args.dir);

    let package_info = read_package_json(&args.pkg)?;
    let dep_info = analyze_project(project_base_dir);

    let issues = check_dependency_graph(dep_info, package_info);
    print_dependency_issues(&issues);

    Ok(())
}
