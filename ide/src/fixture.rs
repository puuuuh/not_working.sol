use std::path::PathBuf;
use std::sync::Arc;
use tracing::warn;
use base_db::File;
use syntax::TextSize;

pub(crate) struct TestFixture {
    pub files: Vec<(String, Arc<str>)>,
    pub position: Option<TextSize>
}

impl TestFixture {
    pub fn parse(data: &str) -> Self {
        if let Some(t) = data.find("$0") {
            let data = data[..t].to_string() + &data[t+2..];
            Self {
                files: vec![("test_file.sol".to_owned(), Arc::from(data.as_str()))],
                position: Some(TextSize::new(t as _))
            }
        } else {
            Self {
                files: vec![("test_file.sol".to_string(), Arc::from(data))],
                position: None
            }
        }
    }
}