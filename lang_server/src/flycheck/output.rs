use std::collections::HashMap;

#[derive(serde::Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct SourceLocation {
    pub file: String,
    pub start: u32,
    pub end: u32,
}

#[derive(serde::Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Diagnostic {
    pub source_location: SourceLocation,
    pub message: String,
}

#[derive(serde::Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct InputFile {
    pub content: String,
}

#[derive(serde::Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Input {
    pub sources: HashMap<String, InputFile>,
}

#[derive(serde::Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct BuildInfo {
    pub input: Input,
}

#[derive(serde::Deserialize, Debug)]
pub struct Output {
    pub errors: Vec<Diagnostic>,
    pub build_infos: Vec<BuildInfo>,
}
