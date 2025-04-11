mod path;

use std::sync::Arc;
use dashmap::{DashMap, Entry};
use salsa::Database;
pub use crate::path::{AnchoredPath, VfsPath};

#[salsa::input(debug)]
pub struct File {
    pub exists: bool,
    pub content: Arc<str>,
}

#[derive(Default)]
pub struct Vfs {
    files: DashMap<VfsPath, File>,
    paths: DashMap<File, VfsPath>,
}


impl Vfs {
    pub fn new() -> Self {
        Self::with_roots(Default::default())
    }

    pub fn with_roots(roots: Vec<VfsPath>) -> Self {
        Self {
            paths: Default::default(),
            files: Default::default()
        }
    }

    pub fn with_files(db: &dyn Database, files: impl Iterator<Item = (VfsPath, Arc<str>)>) -> Self {
        let t = Self::new();
        for (p, content) in files {
            let f = File::new(db, true, content);
            t.paths.insert(f, p.clone());
            t.files.insert(p, f);
        }
        t
    }

    pub fn file(&self, db: &dyn Database, path: &VfsPath) -> File{
        match self.files.entry(path.clone()) {
            Entry::Occupied(f) => {
                *f.get()
            }
            Entry::Vacant(f) => {
                let t = match &path {
                    VfsPath::Path(p) => {
                        let (content, exists): (Arc<str>, bool) = if let Ok(content) = std::fs::read_to_string(p) {
                            (Arc::from(&*content), true)
                        } else {
                            (Arc::from(""), false)
                        };
                        File::new(db, exists, content)
                    }
                    VfsPath::Virtual(_) => {
                        File::new(db, false, Default::default())
                    }
                };
                self.paths.insert(t, path.clone());
                f.insert(t);

                t
            }
        }
    }

    pub fn path(&self, file: File) -> VfsPath {
        self.paths.get(&file).unwrap().clone()
    }

    pub fn resolve_path(&self, db: &dyn Database, path: &AnchoredPath, roots: &[VfsPath]) -> Option<VfsPath> {
        let parent = self.paths.get(&path.parent)?;
        if path.path.starts_with(".") {
            let joined = parent.parent()?.join(&path.path)?;

            if self.files.contains_key(&joined) {
                return Some(joined);
            }
            if let VfsPath::Path(p) = &joined {
                db.report_untracked_read();
                if p.exists() {
                    return Some(joined)
                }
            }
        } else {
            for root in roots {
                let Some(joined) = root.join(&path.path) else { continue };
                if self.files.contains_key(&joined) {
                    return Some(joined);
                }
                if let VfsPath::Path(p) = &joined {
                    db.report_untracked_read();
                    if p.exists() {
                        return Some(joined)
                    }
                }
            }
        }

        None
    }
}


#[cfg(test)]
mod tests {
}
