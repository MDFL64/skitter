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

mod vm;

mod hir_compiler;
mod bytecode_select;
mod layout;
mod cli;
mod test;
mod abi;
mod ir;
mod types;
mod items;
mod rustc_worker;

use clap::Parser;
use rustc_worker::RustCWorker;
use vm::VM;

// doesn't seem to make a huge difference, todo more tests
//use mimalloc::MiMalloc;
//#[global_allocator]
//static GLOBAL: MiMalloc = MiMalloc;

fn main() {
    
    let args = cli::CliArgs::parse();

    let vm = VM::new();
    
    run(args,&vm);
}

fn run<'vm>(args: cli::CliArgs, vm: &'vm VM<'vm>) {
    std::thread::scope(|scope| {
        
        let worker = RustCWorker::new(args, scope, &vm);
        //vm.add_worker(worker);

        println!("ready");
        loop {}

        //vm.run_crate(args);
    });
}


    /*let root_crate = IRCrateLazy::new(args.clone());

    {
        let start = Instant::now();
        let main_id = root_crate.find("::main".into());
        println!("=> {:?} {:?}",main_id,start.elapsed());
    }

    {
        let start = Instant::now();
        let main_id = root_crate.find("::main".into());
        println!("=> {:?} {:?}",main_id,start.elapsed());
    }*/

    /*let source_root = args.file_name;

    if args.test {
        test::test(&source_root);
    }*/

    //println!("? {}",source_root);

    //let source_root = "/home/cogg/.rustup/toolchains/nightly-2023-04-12-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/lib.rs";

    /*let out = process::Command::new("rustc")
        .arg("--print=sysroot")
        .current_dir(".")
        .output()
        .unwrap();
    let sysroot = str::from_utf8(&out.stdout).unwrap().trim();*/

    /*let self_profile = if args.profile { config::SwitchWithOptPath::Enabled(None) } else { config::SwitchWithOptPath::Disabled };

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

                let mut vm = vm::VM::new(tcx);
                vm.is_verbose = args.verbose;

                let main_did = find_main(tcx).expect("no main");
                let main_subs = tcx.mk_substs(&[]);

                let func = vm.get_func(main_did.into(),main_subs);
                vm.call(&func,0);
            })
        });
    });*/

/*fn find_main(tcx: TyCtxt) -> Option<LocalDefId> {
    let hir = tcx.hir();

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
}*/
