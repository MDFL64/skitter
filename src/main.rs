#![feature(rustc_private)]
#![feature(drain_filter)]

extern crate rustc_ast_pretty;
extern crate rustc_driver;
extern crate rustc_error_codes;
extern crate rustc_errors;
extern crate rustc_hash;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_middle;
extern crate rustc_feature;
extern crate rustc_mir_dataflow;
extern crate rustc_ast;
extern crate rustc_abi;
extern crate rustc_metadata;

mod vm;

mod hir_compiler;
mod bytecode_select;
mod cli;
mod test;
mod abi;
mod ir;
mod types;
mod items;
mod rustc_worker;

use std::process;

use clap::Parser;
use vm::VM;

use crate::{items::{ItemPath, ExternCrate}};

// seems neutral or slower than the system allocator (wsl), todo more tests
//use mimalloc::MiMalloc;
//#[global_allocator]
//static GLOBAL: MiMalloc = MiMalloc;

fn main() {

    set_panic_handler();

    let args = cli::CliArgs::parse();

    if args.test {
        test::test(&args.file_name);
    }

    let mut vm = VM::new();
    vm.is_verbose = args.verbose;
    
    std::thread::scope(|scope| {
        
        let mut extern_crates = Vec::new();

        if args.core {
            let sysroot = get_sysroot();
            let core_root = format!("{}/lib/rustlib/src/rust/library/core/src/lib.rs",sysroot);

            // make sure core root exists
            assert!(std::path::Path::new(&core_root).exists());

            let core_crate = vm.add_worker(&core_root,vec!(), scope);
            vm.wait_for_setup(core_crate);
            extern_crates.push(ExternCrate{
                id: core_crate,
                name: "core".to_owned()
            });
        }

        let main_crate = vm.add_worker(&args.file_name,extern_crates, scope);
        vm.wait_for_setup(main_crate);

        let main_path = ItemPath::new_value("::main".to_owned());

        let main_item = vm
            .get_crate_items(main_crate)
            .find_by_path(&main_path)
            .expect("no main found");

        let main_fn = main_item.get_function(&[]);

        vm.call(&main_fn,0);

        process::exit(0)
    });
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
