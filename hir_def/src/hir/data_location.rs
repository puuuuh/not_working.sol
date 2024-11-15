use syntax::ast::nodes;

#[derive(Clone, Eq, PartialEq, Debug, Hash, salsa::Update, derive_more::Display)]
pub enum DataLocation {
    #[display("memory")]
    Memory,
    #[display("storage")]
    Storage,
    #[display("calldata")]
    Calldata,
}

impl From<nodes::DataLocation> for DataLocation {
    fn from(value: nodes::DataLocation) -> Self {
        if value.calldata_token().is_some() {
            Self::Calldata
        } else if value.memory_token().is_some() {
            Self::Memory
        } else {
            Self::Storage
        }
    }
}
