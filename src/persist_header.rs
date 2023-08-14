use std::{
    error::Error,
    ffi::OsStr,
    path::{Path, PathBuf},
};

use base64::Engine;
use rustc_middle::ty::TyCtxt;

use crate::persist::{Persist, PersistReader, PersistWriter};

const MAGIC: &str = "SKITTER-CRATE\n";
const BUILD_ID: &str = "TODO"; //include!(concat!(env!("OUT_DIR"), "/build_id.rs"));

pub fn persist_header_write(writer: &mut PersistWriter) {
    writer.write_bytes(MAGIC.as_bytes());
    writer.write_str(BUILD_ID);
}

pub fn persist_header_read(reader: &mut PersistReader) -> Result<(), String> {
    let magic = reader.read_bytes(MAGIC.len());
    if magic != MAGIC.as_bytes() {
        return Err("bad cache header -- incorrect magic".to_owned());
    }

    let build_id = reader.read_str();
    if build_id != BUILD_ID {
        return Err("bad cache header -- bad build id".to_owned());
    }

    Ok(())
}

#[derive(Debug, PartialEq)]
pub struct PersistCrateHeader {
    pub crate_name: String,
    pub files: Vec<FileVersionEntry>,
}

#[derive(Debug, PartialEq)]
pub struct FileVersionEntry {
    pub path: PathBuf,
    pub size: u64,
    pub time: u64,
}

impl PersistCrateHeader {
    pub fn from_rustc<'tcx>(tcx: TyCtxt<'tcx>) -> Result<Self, Box<dyn Error>> {
        use rustc_middle::dep_graph::DepContext;

        let rustc_files = tcx.sess().source_map().files();

        let mut files = Vec::new();

        for file in rustc_files.iter() {
            if file.cnum == rustc_hir::def_id::LOCAL_CRATE {
                if let rustc_span::FileName::Real(file_name) = &file.name {
                    let path = file_name
                        .local_path()
                        .ok_or_else(|| "no local path".to_owned())?;
                    let path = std::fs::canonicalize(path)?;

                    let meta = std::fs::metadata(&path)?;

                    let size = meta.len();

                    let time = meta
                        .modified()?
                        .duration_since(std::time::UNIX_EPOCH)?
                        .as_millis()
                        .try_into()?;

                    let entry = FileVersionEntry { path, size, time };

                    files.push(entry);
                } else {
                    panic!();
                }
            }
        }

        Ok(PersistCrateHeader {
            crate_name: tcx.crate_name(rustc_hir::def_id::LOCAL_CRATE).to_string(),
            files,
        })
    }

    pub fn cache_file_path(&self) -> PathBuf {
        cache_file_path(&self.crate_name, &self.files[0].path)
    }

    pub fn validate(&self) -> Result<(), Box<dyn Error>> {
        for file in &self.files {
            let meta = std::fs::metadata(&file.path)?;

            let size = meta.len();

            let time: u64 = meta
                .modified()?
                .duration_since(std::time::UNIX_EPOCH)?
                .as_millis()
                .try_into()?;

            if size != file.size || time != file.time {
                return Err(format!("file {:?} was modified", file.path).into());
            }
        }
        Ok(())
    }
}

impl<'vm> Persist<'vm> for PersistCrateHeader {
    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        let crate_name = Persist::persist_read(reader);
        let files = Persist::persist_read(reader);

        Self { crate_name, files }
    }

    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        self.crate_name.persist_write(writer);
        self.files.persist_write(writer);
    }
}

impl<'vm> Persist<'vm> for FileVersionEntry {
    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        let path = Persist::persist_read(reader);
        let size = Persist::persist_read(reader);
        let time = Persist::persist_read(reader);

        Self { path, size, time }
    }

    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        self.path.persist_write(writer);

        self.size.persist_write(writer);
        self.time.persist_write(writer);
    }
}

/// Computes the name used for the resulting cache file.
/// Only uses the crate name and root source path.
/// In the future it should use other build config.
pub fn cache_file_path(crate_name: &str, root_path: &Path) -> PathBuf {
    let mut hash = md5::Context::new();

    hash.consume(root_path.to_string_lossy().as_bytes());

    let hash = hash.compute();
    let hash = base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(*hash);

    format!("./cache/{}-{}", crate_name, hash).into()
}
