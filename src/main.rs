mod parser;

use anyhow::{Context, Result};
use lazy_static::lazy_static;
use parser::{DepParser, JsParser, SvelteParser, TsParser, VueParser};
use serde::Deserialize;
use std::{
    collections::{HashMap, HashSet},
    fs,
    path::Path,
};
use walkdir::WalkDir;

const IGNORE_CONTENTS: &str = include_str!("exclude_list.txt");

lazy_static! {
    static ref IGNORE_FILES: HashSet<String> = {
        IGNORE_CONTENTS
            .lines()
            .map(|line| line.trim())
            .filter(|line| !line.is_empty() && !line.starts_with('#'))
            .map(|line| line.to_string())
            .collect()
    };
}

#[derive(Deserialize, Debug)]
struct PackageJson {
    dependencies: Option<HashMap<String, String>>,
    #[serde(rename = "devDependencies")]
    dev_dependencies: Option<HashMap<String, String>>,
    #[serde(rename = "peerDependencies")]
    peer_dependencies: Option<HashMap<String, String>>,
    #[serde(rename = "optionalDependencies")]
    optional_dependencies: Option<HashMap<String, String>>,
}

#[derive(Debug, Default)]
struct PackageDependencies {
    dependencies: HashSet<String>,
    dev_dependencies: HashSet<String>,
    peer_dependencies: HashSet<String>,
    optional_dependencies: HashSet<String>,
}

// Checks if an import path matches a package dependency
fn is_dependency_used(import_path: &str, package_name: &str) -> bool {
    // Exact match
    if import_path == package_name {
        return true;
    }

    // Subpath imports (e.g., "react/jsx-runtime" matches "react")
    if import_path.starts_with(&format!("{}/", package_name)) {
        return true;
    }

    // Handle scoped packages (e.g., "@babel/core")
    // TODO: Fix this as '@' could be used as an import alais
    if package_name.starts_with('@') && import_path.starts_with(package_name) {
        return true;
    }

    false
}

fn extract_dependency_names(deps_map: Option<HashMap<String, String>>) -> HashSet<String> {
    deps_map
        .map(|map| map.into_keys().collect())
        .unwrap_or_default()
}

fn read_package_json_dependencies<P: AsRef<Path>>(path: P) -> Result<PackageDependencies> {
    let content = fs::read_to_string(&path).with_context(|| {
        format!(
            "Failed to read package.json from {}",
            path.as_ref().display()
        )
    })?;

    let package_data: PackageJson = serde_json::from_str(&content).with_context(|| {
        format!(
            "Failed to parse package.json from {}",
            path.as_ref().display()
        )
    })?;

    Ok(PackageDependencies {
        dependencies: extract_dependency_names(package_data.dependencies),
        dev_dependencies: extract_dependency_names(package_data.dev_dependencies),
        peer_dependencies: extract_dependency_names(package_data.peer_dependencies),
        optional_dependencies: extract_dependency_names(package_data.optional_dependencies),
    })
}

fn process_source_files(dir: &str) -> Result<HashSet<String>> {
    let mut all_dependencies_in_code = HashSet::new();

    for entry_result in WalkDir::new(dir).into_iter().filter_entry(|e| {
        let should_ignore = e.path().components().any(|component| {
            if let Some(name_osstr) = component.as_os_str().to_str() {
                IGNORE_FILES.contains(name_osstr)
            } else {
                false
            }
        });

        !should_ignore
    }) {
        let entry = match entry_result {
            Ok(e) => e,
            Err(err) => {
                eprintln!("Error accessing entry: {}", err);
                continue;
            }
        };

        if entry.file_type().is_file() {
            let path = entry.path();
            if let Some(ext_os_str) = path.extension() {
                if let Some(ext) = ext_os_str.to_str() {
                    let dependencies_result = match ext {
                        "js" | "mjs" | "cjs" | "jsx" => JsParser::parse(path),
                        "ts" | "mts" | "cts" | "tsx" => TsParser::parse(path),
                        "vue" => VueParser::parse(path),
                        "svelte" => SvelteParser::parse(path),
                        _ => continue,
                    };

                    match dependencies_result {
                        Ok(deps) => {
                            all_dependencies_in_code.extend(deps);
                        }
                        Err(e) => {
                            eprintln!("Error parsing file {}: {:?}", path.display(), e);
                        }
                    }
                }
            }
        }
    }

    Ok(all_dependencies_in_code)
}

fn find_used_dependencies(
    dependencies_in_code: &HashSet<String>,
    package_dependencies: &HashSet<String>,
) -> HashSet<String> {
    let mut found_deps = HashSet::new();

    for import_path in dependencies_in_code {
        for package_dep in package_dependencies {
            if is_dependency_used(import_path, package_dep) {
                found_deps.insert(package_dep.clone());
            }
        }
    }

    found_deps
}

fn print_unused_dependencies(dep_type: &str, unused_deps: &[&String]) {
    if !unused_deps.is_empty() {
        println!("=== Found unused {} ===", dep_type);
        for dep in unused_deps {
            println!(" - {}", dep);
        }
    }
}

fn get_unused_dependencies<'a>(
    package_deps: &'a HashSet<String>,
    found_deps: &'a HashSet<String>,
) -> Vec<&'a String> {
    let mut unused: Vec<&'a String> = package_deps.difference(found_deps).collect();
    unused.sort_unstable();
    unused
}

// TODO: Add command line args
fn main() -> Result<()> {
    let current_dir = ".";
    let package_json_path = Path::new(current_dir).join("package.json");

    let package_deps_info = read_package_json_dependencies(&package_json_path)
        .unwrap_or_else(|e| {
            eprintln!(
                "Warning: Could not read or parse package.json at {}: {}. Using empty dependencies.",
                package_json_path.display(),
                e
            );
            PackageDependencies::default()
        });

    let all_dependencies_in_code = process_source_files(current_dir).with_context(|| {
        format!(
            "Failed to process source files in directory '{}'",
            current_dir
        )
    })?;

    // Find which dependencies are actually used
    let found_deps =
        find_used_dependencies(&all_dependencies_in_code, &package_deps_info.dependencies);

    let found_dev_deps = find_used_dependencies(
        &all_dependencies_in_code,
        &package_deps_info.dev_dependencies,
    );

    let found_peer_deps = find_used_dependencies(
        &all_dependencies_in_code,
        &package_deps_info.peer_dependencies,
    );

    let found_optional_deps = find_used_dependencies(
        &all_dependencies_in_code,
        &package_deps_info.optional_dependencies,
    );

    // Calculate unused dependencies
    let unused_deps = get_unused_dependencies(&package_deps_info.dependencies, &found_deps);

    let unused_dev_deps =
        get_unused_dependencies(&package_deps_info.dev_dependencies, &found_dev_deps);

    let unused_peer_deps =
        get_unused_dependencies(&package_deps_info.peer_dependencies, &found_peer_deps);

    let unused_optional_deps = get_unused_dependencies(
        &package_deps_info.optional_dependencies,
        &found_optional_deps,
    );

    print_unused_dependencies("Dependencies", &unused_deps);
    print_unused_dependencies("Dev Dependencies", &unused_dev_deps);
    print_unused_dependencies("Peer Dependencies", &unused_peer_deps);
    print_unused_dependencies("Optional Dependencies", &unused_optional_deps);

    if unused_deps.is_empty()
        && unused_dev_deps.is_empty()
        && unused_peer_deps.is_empty()
        && unused_optional_deps.is_empty()
    {
        println!("✅ No unused dependencies found!");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_dependency_used() {
        // Exact match
        assert!(is_dependency_used("react", "react"));

        // Subpath imports
        assert!(is_dependency_used("react/jsx-runtime", "react"));
        assert!(is_dependency_used("lodash/debounce", "lodash"));

        // Scoped packages
        assert!(is_dependency_used("@babel/core", "@babel/core"));
        assert!(is_dependency_used("@babel/core/lib/parser", "@babel/core"));

        // Should not match
        assert!(!is_dependency_used("react-dom", "react"));
        assert!(!is_dependency_used("react", "react-dom"));
        assert!(!is_dependency_used("some-package", "other-package"));
    }

    #[test]
    fn test_extract_dependency_names() {
        let mut deps = HashMap::new();
        deps.insert("react".to_string(), "^18.0.0".to_string());
        deps.insert("lodash".to_string(), "^4.17.21".to_string());

        let result = extract_dependency_names(Some(deps));
        assert_eq!(result.len(), 2);
        assert!(result.contains("react"));
        assert!(result.contains("lodash"));

        let empty_result = extract_dependency_names(None);
        assert!(empty_result.is_empty());
    }
}
