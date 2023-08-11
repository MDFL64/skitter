#![feature(rustc_private)]
#![feature(drain_filter)]

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
mod cli;
mod crate_provider;
mod ir;
mod items;
mod lazy_collections;
mod persist_header;
mod persist;
mod rustc_worker;
mod test;
mod types;
mod cache_provider;

use std::process;

use clap::Parser;
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

    if args.test {
        let mut global_args = Vec::new();
        if args.save {
            global_args.push("--save");
        }
        test::test(&args.file_name,&global_args);
    }

    let vm: &VM = Box::leak(Box::new(VM::new(args.core,args.verbose)));
    // HACK: this must be initialized ASAP so common types have correct persist IDs
    vm.common_types();

    
    let mut extern_crates = Vec::new();

    if args.core {
        let sysroot = get_sysroot();
        let core_root = format!("{}/lib/rustlib/src/rust/library/core/src/lib.rs", sysroot);

        // make sure core root exists
        assert!(std::path::Path::new(&core_root).exists());

        let core_crate = vm.add_rustc_provider(RustCWorkerConfig {
            source_root: core_root,
            extern_crates: vec![],
            is_core: true,
            save_file: false,
        });

        assert!(Some(core_crate) == vm.core_crate);

        extern_crates.push(ExternCrate {
            id: core_crate,
            name: "core".to_owned(),
        });
    }

    let main_crate = if args.load {
        vm.add_cache_provider(&args.file_name)
    } else {
        vm.add_rustc_provider(RustCWorkerConfig {
            source_root: args.file_name,
            extern_crates,
            is_core: false,
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

// currently used only to locate core lib sources
fn get_sysroot() -> String {
    let out = process::Command::new("rustc")
        .arg("--print=sysroot")
        .current_dir(".")
        .output()
        .unwrap();
    std::str::from_utf8(&out.stdout).unwrap().trim().to_owned()
}
