use vfs::VfsPath;

#[salsa::input]
pub struct Project {
    #[return_ref]
    pub import_paths: Vec<VfsPath>,
    #[return_ref]
    pub remappings: Vec<(String, String)>,
}