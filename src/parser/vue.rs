use super::{DepParser, JsParser, TsParser}; // Import JsParser and TsParser
use once_cell::sync::Lazy;
use regex::Regex;
use std::{collections::HashSet, error::Error, fs, path::Path};

// Captures contents and attributes of script tags
static SCRIPT_TAG_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"<script(?P<attrs>[^>]*)>((?:.|\n)*?)</script>").unwrap());

// Checks for lang="ts" or lang="typescript" in attributes
static LANG_TS_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r#"\blang\s*=\s*["'](?:ts|typescript)["']"#).unwrap());

pub struct VueParser;

impl DepParser for VueParser {
    fn parse<P: AsRef<Path>>(path: P) -> Result<Vec<String>, Box<dyn Error>> {
        let vue_content = fs::read_to_string(path.as_ref())?;
        let mut aggregated_dependencies = HashSet::new();

        for script_cap in SCRIPT_TAG_REGEX.captures_iter(&vue_content) {
            let script_content = script_cap.get(2).map_or("", |m| m.as_str());
            let attrs = script_cap.name("attrs").map_or("", |m| m.as_str());

            let current_script_deps = if LANG_TS_REGEX.is_match(attrs) {
                TsParser::parse_script_content(script_content)
            } else {
                JsParser::parse_script_content(script_content)
            };

            for dep in current_script_deps {
                aggregated_dependencies.insert(dep);
            }
        }

        Ok(aggregated_dependencies.into_iter().collect())
    }
}
