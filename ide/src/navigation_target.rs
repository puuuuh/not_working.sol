use syntax::TextRange;
use vfs::File;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct NavigationTarget {
    pub file: File,

    pub full_range: TextRange,
    pub focus_range: TextRange,
}