use std::{time::Duration, sync::{Mutex, Barrier, Arc, OnceLock}};

use rustc_session::config;

use crate::cli::CliArgs;

trait IRCrate {
    fn find(&self, path: &str) -> ItemId;
}

struct ItemId(u32);

/////////////////////////

pub struct IRCrateLazy {
    sender: std::sync::mpsc::Sender<Arc<IRCrateCmd>>
}

struct IRCrateCmd {
    kind: IRCrateCmdKind,
    complete: Barrier
}

enum IRCrateCmdKind {
    Find{path: String, res: OnceLock<ItemId>}
}

impl IRCrateLazy {
    pub fn new(args: CliArgs) -> IRCrateLazy {

        let (sender,recv) = std::sync::mpsc::channel();

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
            input: config::Input::File(args.file_name.into()),
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

        
        std::thread::spawn(move || {
            rustc_interface::run_compiler(config, |compiler| {
                compiler.enter(|queries| {
                    queries.global_ctxt().unwrap().enter(|tcx| {
        
                        loop {
                            println!("yo");
    
                            std::thread::sleep(Duration::from_secs(1));
                        }
                        /*let mut vm = vm::VM::new(tcx);
                        vm.is_verbose = args.verbose;
        
                        let main_did = find_main(tcx).expect("no main");
                        let main_subs = tcx.mk_substs(&[]);
        
                        let func = vm.get_func(main_did.into(),main_subs);
                        vm.call(&func,0);*/
                    })
                });
            });
            println!("exit");
        });

        IRCrateLazy{
            sender
        }
    }

    fn call(&self, cmd: IRCrateCmdKind) {
        let cmd = IRCrateCmd {
            kind: cmd,
            complete: Barrier::new(2)
        };
        self.sender.send(&cmd);
        cmd.complete.wait();
    }
}

impl<'a> IRCrate for IRCrateLazy<'a> {
    fn find(&self, path: &str) -> ItemId {
        self.call(IRCrateCmdKind::Find{ path: path.into(), res: None });
        panic!();
    }
}
