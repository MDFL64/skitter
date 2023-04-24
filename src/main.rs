#![feature(rustc_private)]

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

mod vm;
mod mir_compiler;
mod layout;
mod cli;
mod test;
mod abi;

use rustc_session::config;
use rustc_span::source_map;
use rustc_middle::ty::WithOptConstParam;
use rustc_middle::ty::TyCtxt;
use rustc_hir::ItemKind;
use rustc_hir::def_id::LocalDefId;

use clap::Parser;

use crate::mir_compiler::MirCompiler;
use crate::vm::exec::exec_main;

fn main() {
    
    let args = cli::CliArgs::parse();

    let source_root = args.file_name;

    if args.test {
        test::test(&source_root);
    }

    //println!("? {}",source_root);

    //let source_root = "/home/cogg/.rustup/toolchains/nightly-2023-04-12-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/lib.rs";

    /*let out = process::Command::new("rustc")
        .arg("--print=sysroot")
        .current_dir(".")
        .output()
        .unwrap();
    let sysroot = str::from_utf8(&out.stdout).unwrap().trim();*/

    let self_profile = if args.profile { config::SwitchWithOptPath::Enabled(None) } else { config::SwitchWithOptPath::Disabled };

    let config = rustc_interface::Config {
        opts: config::Options {
            //maybe_sysroot: Some(path::PathBuf::from(sysroot)), //Some(path::PathBuf::from("./bunk/")), // sysroot
            edition: rustc_span::edition::Edition::Edition2021,
            unstable_features: rustc_feature::UnstableFeatures::Cheat,
            cg: config::CodegenOptions {
                overflow_checks: Some(false),
                ..config::CodegenOptions::default()
            },
            unstable_opts: config::UnstableOptions {
                self_profile,
                ..config::UnstableOptions::default()
            },
            ..config::Options::default()
        },
        input: config::Input::File(source_root.into()),
        crate_cfg: rustc_hash::FxHashSet::default(),
        crate_check_cfg: config::CheckCfg::default(),
        // (Some(Mode::Std), "backtrace_in_libstd", None),
        output_dir: None,
        output_file: None,
        file_loader: None,
        locale_resources: rustc_driver::DEFAULT_LOCALE_RESOURCES,
        lint_caps: rustc_hash::FxHashMap::default(),
        parse_sess_created: None,
        register_lints: None,
        override_queries: None,
        make_codegen_backend: None,
        registry: rustc_errors::registry::Registry::new(&rustc_error_codes::DIAGNOSTICS),
    };

    rustc_interface::run_compiler(config, |compiler| {
        compiler.enter(|queries| {
            queries.global_ctxt().unwrap().enter(|tcx| {

                let skitter = SkitterCompiler::new(tcx);

                let main_did = skitter.find_main().expect("no main");

                let mir = tcx.mir_built(WithOptConstParam::unknown(main_did)).borrow();
                
                let func = MirCompiler::compile(tcx,&mir);

                exec_main(&func.instr);
            })
        });
    });
}

struct SkitterCompiler<'a> {
    tcx: TyCtxt<'a>
}

impl<'a> SkitterCompiler<'a> {
    pub fn new(tcx: TyCtxt<'a>) -> Self {
        SkitterCompiler{
            tcx
        }
    }

    pub fn find_main(&self) -> Option<LocalDefId> {
        let hir = self.tcx.hir();

        let root = hir.root_module();

        for child_id in root.item_ids {
            let child = hir.item(*child_id);
            if child.ident.as_str() == "main" {
                if let ItemKind::Fn(..) = child.kind {
                    return Some(child_id.owner_id.def_id);
                }
            }
        }
        None
    }
}
