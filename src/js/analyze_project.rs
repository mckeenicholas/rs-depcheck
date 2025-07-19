use std::{
    collections::HashMap,
    error::Error,
    fs::File,
    io::Read,
    path::{Path, PathBuf},
};

use oxc::{
    allocator::Allocator,
    parser::{Parser, ParserReturn},
    span::SourceType,
};
use walkdir::{DirEntry, WalkDir};

use crate::js::js_parser::{ModuleAnalysis, ModuleAnalyzer};
use crate::vue::analyze_vue::analyze_vue_template;

pub const TARGET_EXTENSIONS: &[&str] = &["js", "jsx", "ts", "tsx", "vue", "svelte"];
const SKIP_DIRS: &[&str] = &["node_modules", "dist", "build"];

fn in_ignore_list(entry: &DirEntry) -> bool {
    entry
        .file_name()
        .to_str()
        .map(|s| (s != "." && s.starts_with(".")) || SKIP_DIRS.contains(&s))
        .unwrap_or(false)
}

pub fn analyze_project<P: AsRef<Path>>(root_dir: P) -> HashMap<PathBuf, ModuleAnalysis> {
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
                eprintln!("Error: {e}");
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
        let results = analyze_file(&allocator, path);
        match results {
            Ok(module_analysis) => {
                src_file_info.insert(path.to_owned(), module_analysis);
            }
            Err(e) => {
                eprintln!("Failed to analyze file: {}", path.display());
                eprintln!("    Error: {}", e);

                // Try to extract more detailed error information if it's a parsing error
                let error_str = e.to_string();
                if error_str.contains("Error") || error_str.contains("Diagnostic") {
                    // Split multi-line errors and print each on a separate line with proper indentation
                    for line in error_str.lines() {
                        if !line.trim().is_empty() {
                            eprintln!("    {}", line.trim());
                        }
                    }
                }
            }
        }

        // Reset the allocator to free memory for the next file
        allocator.reset();
    }

    src_file_info
}

fn analyze_file<P: AsRef<Path>>(
    allocator: &Allocator,
    file_path: P,
) -> Result<ModuleAnalysis, Box<dyn Error>> {
    let mut file_handle = File::open(&file_path)?;
    let mut file_contents = String::new();
    file_handle.read_to_string(&mut file_contents)?;

    let path_str = file_path.as_ref().to_str().unwrap_or("").to_lowercase();

    if path_str.ends_with(".vue") {
        return analyze_vue_template(&allocator, &file_contents);
    } else {
        let source_type = SourceType::from_path(&file_path)?;
        return analyze_js(&allocator, file_contents, source_type);
    }
}

pub fn analyze_js(
    allocator: &Allocator,
    file_contents: String,
    source_type: SourceType,
) -> Result<ModuleAnalysis, Box<dyn Error>> {
    let parser = Parser::new(allocator, &file_contents, source_type);
    let ParserReturn {
        program,
        errors,
        panicked,
        module_record,
        ..
    } = parser.parse();

    if panicked {
        let error_string = errors
            .iter()
            .map(|diag| {
                // OXC diagnostics contain span information that we can use
                let message = &diag.message;

                if let Some(ref spans) = diag.labels {
                    if let Some(label) = spans.first() {
                        // Calculate line and column numbers from the span
                        let source_text = &file_contents;
                        let start_pos = label.offset() as usize;
                        let lines_before = &source_text[..start_pos.min(source_text.len())];
                        let line_num = lines_before.matches('\n').count() + 1;
                        let last_newline = lines_before.rfind('\n').map_or(0, |pos| pos + 1);
                        let col_num = start_pos - last_newline + 1;

                        format!("{}:{}: {}", line_num, col_num, message)
                    } else {
                        format!("{}", message)
                    }
                } else {
                    format!("{}", message)
                }
            })
            .collect::<Vec<String>>()
            .join("\n");
        return Err(error_string.into());
    }

    let mut analyzer = ModuleAnalyzer::default();
    let analysis = analyzer.analyze_with_module_record(&program, &module_record);

    Ok(analysis)
}
