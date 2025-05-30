use std::error::Error;
use std::path::Path;

pub mod js;
pub mod svelte;
pub mod ts;
pub mod vue;

pub use js::JsParser;
pub use svelte::SvelteParser;
pub use ts::TsParser;
pub use vue::VueParser;

pub trait DepParser {
    fn parse<P: AsRef<Path>>(path: P) -> Result<Vec<String>, Box<dyn Error>>;
}
