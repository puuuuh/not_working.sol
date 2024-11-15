use vfs::VfsPath;

#[salsa::input]
pub struct Project {
    #[return_ref]
    pub import_paths: Vec<VfsPath>,
}