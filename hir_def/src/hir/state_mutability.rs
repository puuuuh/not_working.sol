use std::fmt::{Display, Formatter};

#[derive(Clone, Eq, PartialEq, Debug, Hash, salsa::Update)]
pub enum StateMutability {
    Pure,
    View,
    Payable,
    NonPayable,
}

impl Display for StateMutability {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            StateMutability::Pure => " pure",
            StateMutability::View => " view",
            StateMutability::Payable => " payable",
            StateMutability::NonPayable => "",
        })
    }
}
