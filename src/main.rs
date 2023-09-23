#![feature(rustc_private)]
#![feature(drain_filter)]
#![feature(lazy_cell)]

extern crate rustc_abi;
extern crate rustc_ast;
extern crate rustc_ast_pretty;
extern crate rustc_driver;
extern crate rustc_error_codes;
extern crate rustc_errors;
extern crate rustc_feature;
extern crate rustc_hash;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_metadata;
extern crate rustc_middle;
extern crate rustc_mir_dataflow;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;

mod vm;

mod abi;
mod builtins;
mod bytecode_compiler;
mod bytecode_select;
mod cache_provider;
mod cli;
mod closure;
mod crate_provider;
mod ir;
mod items;
mod lazy_collections;
mod persist;
mod persist_header;
mod profiler;
mod rustc_worker;
mod test;
mod types;

use std::{
    cell::LazyCell,
    ffi::{OsStr, OsString},
    path::{Path, PathBuf},
    process,
};

use clap::Parser;
use profiler::profile;
use types::SubList;
use vm::VM;

use crate::{
    items::{ExternCrate, ItemPath},
    rustc_worker::RustCWorkerConfig,
};

// seems neutral or slower than the system allocator (wsl), todo more tests
//use mimalloc::MiMalloc;
//#[global_allocator]
//static GLOBAL: MiMalloc = MiMalloc;

fn main() {
    set_panic_handler();

    let args = cli::CliArgs::parse();

    profiler::profile("top", || {
        if args.repeat {
            loop {
                run(&args);
            }
        } else {
            run(&args);
        }
    });

    if args.profile {
        profiler::profile_log();
    }
}

fn run(args: &cli::CliArgs) {
    let file_name = Path::new(&args.file_name);

    if args.test {
        /* currently not used
        let mut global_args = Vec::new();

        if args.save {
            global_args.push(OsString::from("--save"));
        }
        if args.load {
            global_args.push(OsString::from("--load"));
        }*/
        test::test(file_name, vec![]);
    }

    let vm: &VM = Box::leak(Box::new(VM::new(args.verbose)));
    // HACK: this must be initialized ASAP so common types have correct persist IDs
    vm.common_types();

    let mut extern_crates = Vec::new();

    /*if args.core {
        let sysroot = get_sysroot();
        let core_root = PathBuf::from(format!(
            "{}/lib/rustlib/src/rust/library/core/src/lib.rs",
            sysroot
        ));

        // make sure core root exists
        assert!(core_root.exists());

        let core_crate = vm.add_rustc_provider(RustCWorkerConfig {
            source_root: core_root,
            extern_crates: vec![],
            save_file: false,
        });

        assert!(Some(core_crate) == vm.core_crate);

        extern_crates.push(ExternCrate {
            id: core_crate,
            name: "core".to_owned(),
        });
    }*/

    let crate_path = CratePath::new(&args.file_name);

    if !crate_path.is_core() {
        let core_path = CratePath::new(OsStr::new("@core"));

        let core_id = vm.add_cache_provider(&core_path).expect("core load failed");

        extern_crates.push(ExternCrate {
            id: core_id,
            name: "core".to_owned(),
        });

        vm.core_crate.set(core_id).unwrap();
    }

    let main_crate = if args.load {
        /*let crate_name = file_name.file_stem().unwrap().to_str().unwrap();

        let source_path = file_name.canonicalize().unwrap();
        let cache_path = cache_file_path(crate_name, &source_path);*/

        vm.add_cache_provider(&crate_path)
            .expect("cache load failed")
    } else {
        vm.add_rustc_provider(RustCWorkerConfig {
            crate_path,
            extern_crates,
            save_file: args.save,
        })
    };

    let main_path = ItemPath::main();

    let main_item = vm
        .crate_provider(main_crate)
        .item_by_path(&main_path)
        .expect("no main found");

    let main_fn = main_item.func_mono(&SubList { list: Vec::new() });

    let thread = vm.make_thread();
    thread.call(&main_fn, 0);
}

/// When any thread panics, close the process.
fn set_panic_handler() {
    let orig_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic_info| {
        orig_hook(panic_info);
        process::exit(1);
    }));
}

const SYSROOT: LazyCell<PathBuf> = LazyCell::new(|| {
    let out = process::Command::new("rustc")
        .arg("--print=sysroot")
        .current_dir(".")
        .output()
        .unwrap();

    std::str::from_utf8(&out.stdout)
        .expect("bad sysroot")
        .trim()
        .into()
});

#[derive(Debug)]
pub struct CratePath {
    pub name: String,
    path: Option<PathBuf>,
}

impl CratePath {
    pub fn new(path: &OsStr) -> Self {
        if let Some(path) = path.to_str() {
            if path.starts_with('@') {
                let name = path[1..].to_owned();

                //let mut path = SYSROOT.clone();
                //path.push(format!("lib/rustlib/src/rust/library/{}/src/lib.rs",name));

                return CratePath { name, path: None };
            }
        }

        let path = Path::new(path).canonicalize().expect("invalid path (1)");
        let name = path
            .file_stem()
            .expect("invalid path (2)")
            .to_str()
            .expect("invalid path (3)")
            .to_owned();

        CratePath {
            name,
            path: Some(path),
        }
    }

    pub fn source_path(&self) -> PathBuf {
        if let Some(path) = &self.path {
            path.clone()
        } else {
            let mut path = SYSROOT.clone();
            path.push(format!(
                "lib/rustlib/src/rust/library/{}/src/lib.rs",
                self.name
            ));
            path
        }
    }

    pub fn is_internal(&self) -> bool {
        self.path.is_none()
    }

    pub fn is_core(&self) -> bool {
        self.is_internal() && self.name == "core"
    }

    pub fn cache_path(&self) -> PathBuf {
        use base64::Engine;

        let mut hash = md5::Context::new();

        // do not hash the the source path for internal crates
        if let Some(path) = &self.path {
            hash.consume(path.to_string_lossy().as_bytes());
        }

        let hash = hash.compute();
        let hash = base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(*hash);

        format!("./cache/{}-{}", self.name, hash).into()
    }
}
