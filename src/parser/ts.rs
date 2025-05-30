use super::DepParser;
use once_cell::sync::Lazy;
use regex::Regex;
use std::{collections::HashSet, error::Error, fs, path::Path};

// Currently, these are the same as the JS Regexs
// TODO: Add TS speific syntax
static STATIC_IMPORT_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r#"import(?:.*?)from\s*['"]([^'"./][^'"]*)['"]"#).unwrap());
static REQUIRE_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r#"require\s*\(\s*['"]([^'"./][^'"]*)['"]\s*\)"#).unwrap());
static DYNAMIC_IMPORT_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r#"import\s*\(\s*['"]([^'"./][^'"]*)['"]\s*\)"#).unwrap());
static SIDE_EFFECT_IMPORT_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r#"import\s+['"]([^'"./][^'"]*)['"];?"#).unwrap());

pub struct TsParser;

impl TsParser {
    pub fn parse_script_content(content: &str) -> HashSet<String> {
        let mut dependencies = HashSet::new();

        for cap in STATIC_IMPORT_REGEX.captures_iter(content) {
            if let Some(module) = cap.get(1) {
                dependencies.insert(module.as_str().to_string());
            }
        }

        for cap in REQUIRE_REGEX.captures_iter(content) {
            if let Some(module) = cap.get(1) {
                dependencies.insert(module.as_str().to_string());
            }
        }

        for cap in DYNAMIC_IMPORT_REGEX.captures_iter(content) {
            if let Some(module) = cap.get(1) {
                dependencies.insert(module.as_str().to_string());
            }
        }

        for cap in SIDE_EFFECT_IMPORT_REGEX.captures_iter(content) {
            if let Some(module) = cap.get(1) {
                dependencies.insert(module.as_str().to_string());
            }
        }
        dependencies
    }
}

impl DepParser for TsParser {
    fn parse<P: AsRef<Path>>(path: P) -> Result<Vec<String>, Box<dyn Error>> {
        let content = fs::read_to_string(path.as_ref())?;
        let dependencies_set = Self::parse_script_content(&content);
        Ok(dependencies_set.into_iter().collect())
    }
}
