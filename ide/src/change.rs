use std::sync::Arc;

use vfs::File;

pub struct ContentChange {
    pub data: String,
    pub range: (isize, isize),
}

pub enum FileChange {
    SetContent {
        data: Arc<str>
    },
    Delete {},
    Create {},
    Rename {
        new_file: File,
    },
}