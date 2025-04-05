mod path;

use std::sync::Arc;
use dashmap::{DashMap, Entry};
use salsa::Database;
pub use crate::path::{AnchoredPath, VfsPath};

#[salsa::input]
pub struct File {
    pub content: Arc<str>,
}

#[derive(Default)]
pub struct Vfs {
    files: DashMap<VfsPath, Option<File>>,
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

    pub fn with_files(files: impl Iterator<Item = (VfsPath, File)>) -> Self {
        let t = Self::new();
        for (p, f) in files {
            t.paths.insert(f, p.clone());
            t.files.insert(p.clone(), Some(f));
        }
        t
    }

    pub fn file(&self, db: &dyn Database, path: &VfsPath) -> Option<File> {
        match self.files.entry(path.clone()) {
            Entry::Occupied(f) => {
                *f.get()
            }
            Entry::Vacant(f) => {
                match &path {
                    VfsPath::Path(p) => {
                        let content = std::fs::read_to_string(p).ok().map(|a| File::new(db, Arc::from(a)));
                        if let Some(f) = &content {
                            self.paths.insert(*f, path.clone());
                        }
                        f.insert(content);
                        content
                    }
                    VfsPath::Virtual(_) => {
                        f.insert(None);
                        None
                    }
                }
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
