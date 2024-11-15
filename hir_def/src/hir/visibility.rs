use std::fmt::{Display, Formatter};

#[derive(Clone, Eq, PartialEq, Debug, Hash, salsa::Update)]
pub enum Visibility {
    Internal,
    External,
    Private,
    Public,
    Unknown,
}

impl Display for Visibility {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Visibility::Internal => " internal",
            Visibility::External => " external",
            Visibility::Private => " private",
            Visibility::Public => " public",
            Visibility::Unknown => "",
        })
    }
}
