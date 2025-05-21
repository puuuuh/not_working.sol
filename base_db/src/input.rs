use vfs::VfsPath;

use crate::BaseDb;

#[salsa::input(singleton)]
pub struct Project {
    #[returns(ref)]
    pub root: VfsPath,
}

#[salsa::tracked]
impl<'db> Project {
    #[salsa::tracked(returns(ref))]
    pub fn remappings(self, db: &'db dyn BaseDb) -> Vec<(String, String)> {
        let Some(root) = self.root(db).join("remappings.txt") else {
            return vec![];
        };
        let mut remappings = vec![];
        if let Some(f) = db.file(&root) {
            for remapping in f.content(db).lines() {
                let remapping = remapping.trim();
                if remapping.is_empty() {
                    continue;
                }
                if let Some((k, v)) = remapping.split_once('=') {
                    remappings.push((k.to_owned(), v.to_owned()))
                }
            }
        }
        return remappings;
    }

    #[salsa::tracked(returns(ref))]
    pub fn import_paths(self, db: &'db dyn BaseDb) -> Vec<VfsPath> {
        let r = self.root(db);
        vec![r.join("src").unwrap(), r.join("dependencies").unwrap(), r.clone()]
    }
}
