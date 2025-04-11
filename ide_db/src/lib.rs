use std::sync::Arc;

use base_db::File;

pub mod defs;


trait FileLineIndex {
    fn line_index<'db>(self, db: &'db dyn base_db::BaseDb) -> Arc<line_index::LineIndex>;
}


#[salsa::tracked]
impl FileLineIndex for File {
    #[salsa::tracked]
    fn line_index(self, db: &dyn base_db::BaseDb) -> Arc<line_index::LineIndex> {
        Arc::new(line_index::LineIndex::new(&*self.content(db)))
    }
}