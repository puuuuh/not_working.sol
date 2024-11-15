use camino::{Utf8Component, Utf8Path, Utf8PathBuf};
use crate::File;

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone)]
pub struct AnchoredPath {
    pub(crate) parent: File,
    pub(crate) path: String,
}

impl AnchoredPath {
    pub fn new(parent: File, path: String) -> Self {
        Self {
            parent,
            path
        }
    }
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Hash)]
pub enum VfsPath {
    Path(Utf8PathBuf),
    Virtual(VirtualPath),
}

impl VfsPath {
    pub fn from_path(data: Utf8PathBuf) -> Self {
        VfsPath::Path(data)
    }

    pub fn from_virtual(data: String) -> Self {
        Self::Virtual(VirtualPath(data))
    }

    pub fn join(&self, path: &str) -> Option<Self> {
        Some(match self {
            VfsPath::Path(pbuf) => {
                VfsPath::from_path(normalize_path(&pbuf.join(path)))
            }
            VfsPath::Virtual(p) => {
                VfsPath::Virtual(p.join(path)?)
            }
        })
    }

    pub fn is_virtual(&self) -> bool {
        matches!(self, VfsPath::Virtual(_))
    }
}

// Taken from https://github.com/rust-lang/rust-analyzer/blob/cf8f950baab30f335917b177536d2d73e0aaa1ae/crates/vfs/src/vfs_path.rs#L334
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Hash)]
pub struct VirtualPath(String);

impl VirtualPath {
    fn starts_with(&self, other: &VirtualPath) -> bool {
        self.0.starts_with(&other.0)
    }

    fn strip_prefix(&self, base: &VirtualPath) -> Option<VirtualPath> {
        None
    }

    fn pop(&mut self) -> bool {
        let pos = match self.0.rfind('/') {
            Some(pos) => pos,
            None => return false,
        };
        self.0 = self.0[..pos].to_string();
        true
    }

    fn join(&self, mut path: &str) -> Option<VirtualPath> {
        if path.starts_with('/') {
            return Some(VirtualPath(path.to_owned()))
        }
        let mut res = self.clone();
        while path.starts_with("../") {
            if !res.pop() {
                return None;
            }
            path = &path["../".len()..];
        }
        path = path.trim_start_matches("./");
        res.0 = format!("{}/{path}", res.0);
        Some(res)
    }

    fn name_and_extension(&self) -> Option<(&str, Option<&str>)> {
        let file_path = if self.0.ends_with('/') { &self.0[..&self.0.len() - 1] } else { &self.0 };
        let file_name = match file_path.rfind('/') {
            Some(position) => &file_path[position + 1..],
            None => file_path,
        };

        if file_name.is_empty() {
            None
        } else {
            let mut file_stem_and_extension = file_name.rsplitn(2, '.');
            let extension = file_stem_and_extension.next();
            let file_stem = file_stem_and_extension.next();

            match (file_stem, extension) {
                (None, None) => None,
                (None | Some(""), Some(_)) => Some((file_name, None)),
                (Some(file_stem), extension) => Some((file_stem, extension)),
            }
        }
    }
}

/// Taken from <https://github.com/rust-lang/cargo/blob/79c769c3d7b4c2cf6a93781575b7f592ef974255/src/cargo/util/paths.rs#L60-L85>
fn normalize_path(path: &Utf8Path) -> Utf8PathBuf {
    let mut components = path.components().peekable();
    let mut ret = if let Some(c @ Utf8Component::Prefix(..)) = components.peek().copied() {
        components.next();
        Utf8PathBuf::from(c.as_str())
    } else {
        Utf8PathBuf::new()
    };

    for component in components {
        match component {
            Utf8Component::Prefix(..) => unreachable!(),
            Utf8Component::RootDir => {
                ret.push(component.as_str());
            }
            Utf8Component::CurDir => {}
            Utf8Component::ParentDir => {
                ret.pop();
            }
            Utf8Component::Normal(c) => {
                ret.push(c);
            }
        }
    }
    ret
}