use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::error::Error;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageJson {
    pub name: Option<String>,
    pub version: Option<String>,
    #[serde(default)]
    pub dependencies: HashMap<String, String>,
    #[serde(rename = "devDependencies", default)]
    pub dev_dependencies: HashMap<String, String>,
    #[serde(rename = "peerDependencies", default)]
    pub peer_dependencies: HashMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageDependencies {
    pub dependencies: Vec<String>,
    pub dev_dependencies: Vec<String>,
    pub peer_dependencies: Vec<String>,
}

impl From<PackageJson> for PackageDependencies {
    fn from(package_json: PackageJson) -> Self {
        PackageDependencies {
            dependencies: package_json.dependencies.keys().cloned().collect(),
            dev_dependencies: package_json.dev_dependencies.keys().cloned().collect(),
            peer_dependencies: package_json.peer_dependencies.keys().cloned().collect(),
        }
    }
}

pub fn read_package_json<P: AsRef<Path>>(path: P) -> Result<PackageDependencies, Box<dyn Error>> {
    let file_handle = File::open(path)?;
    let reader = BufReader::new(file_handle);

    let package_json: PackageJson = serde_json::from_reader(reader)?;

    Ok(package_json.into())
}

// pub fn read_package_json_full<P: AsRef<Path>>(
//     path: Option<&P>,
// ) -> Result<PackageJson, Box<dyn Error>> {
//     let file_path = path
//         .map(|p| p.as_ref())
//         .unwrap_or_else(|| Path::new(DEFAULT_PACKAGE_JSON_PATH));

//     let file_handle = File::open(file_path)?;
//     let reader = BufReader::new(file_handle);

//     let package_json: PackageJson = serde_json::from_reader(reader)?;

//     Ok(package_json)
// }
