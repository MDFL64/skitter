use std::{sync::{Mutex, Barrier, Arc, OnceLock}, collections::HashMap, time::Instant};

use rustc_session::config;
use rustc_middle::ty::{TyCtxt, Ty};
use rustc_hir::ItemKind as HirItemKind;
use rustc_hir::AssocItemKind;

use crate::{cli::CliArgs, ir::{IRFunctionBuilder}, vm::{VM}, types::Type, items::{CrateId, CrateItems, ItemKind, ItemPath, ItemId}};

/////////////////////////

pub struct RustCWorker<'vm> {
    sender: Mutex<std::sync::mpsc::Sender<Box<dyn WorkerCommandDyn>>>,
    pub items: Option<&'vm CrateItems<'vm>>,
    //crate_id: CrateId
}

//type GlobalItemMap<'vm> = HashMap<String,(rustc_hir::def_id::LocalDefId,Item<'vm>)>;
//type LocalItemMap<'vm> = HashMap<rustc_hir::def_id::DefIndex,Item<'vm>>;

pub struct RustCContext<'vm,'tcx> {
    pub items: &'vm CrateItems<'vm>,
    pub vm: &'vm VM<'vm>,
    pub tcx: TyCtxt<'tcx>
}

impl<'vm,'tcx> RustCContext<'vm,'tcx> {
    pub fn type_from_rustc(&self, ty: Ty<'tcx>) -> Type<'vm> {
        self.vm.types.type_from_rustc(ty,self)
    }
}

pub struct WorkerResult<T> {
    complete: Barrier,
    value: Mutex<Option<T>>
}

impl<T> WorkerResult<T> {
    pub fn wait(&self) {
        self.complete.wait();
    }
}

struct WorkerCommand<T,F> {
    func: F,
    result: Arc<WorkerResult<T>>
}

trait WorkerCommandDyn: Send + Sync {
    fn call<'vm>(&self, context: RustCContext);
}

impl<T,F> WorkerCommandDyn for WorkerCommand<T,F> where
    F: Fn(RustCContext)->T + Send + Sync,
    T: Send + Sync
{
    fn call<'vm>(&self, context: RustCContext) {
        let res = (self.func)(context);
        {
            let mut guard = self.result.value.lock().unwrap();
            *guard = Some(res);
        }
        self.result.complete.wait();
    }
}

#[derive(Debug)]
struct ImplItem<'tcx> {
    self_ty: Ty<'tcx>,
    trait_def: Option<rustc_hir::def_id::DefId>,
    children: Vec<(String,ItemId)>
}

impl<'vm> RustCWorker<'vm> {
    pub fn new<'s>(args: CliArgs, scope: &'s std::thread::Scope<'s,'vm>, vm: &'vm VM<'vm>, this_crate: CrateId) -> Self {

        let is_verbose = args.verbose;

        let (sender,recv) =
            std::sync::mpsc::channel::<Box<dyn WorkerCommandDyn>>();

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
        
        scope.spawn(move || {
            rustc_interface::run_compiler(config, |compiler| {
                compiler.enter(move |queries| {
                    queries.global_ctxt().unwrap().enter(|tcx| {

                        let mut items = CrateItems::new(this_crate);

                        let hir = tcx.hir();

                        let t = Instant::now();
                        let hir_items = hir.items();
                        if is_verbose {
                            println!("rustc hir items took {:?}",t.elapsed());
                        }

                        let t = Instant::now();

                        let mut adt_items = Vec::new();
                        let mut impl_items = Vec::new();

                        for item_id in hir_items {
                            let item = hir.item(item_id);
                            match item.kind {
                                // useless to us
                                HirItemKind::Use(..) |
                                HirItemKind::Mod(_) |
                                HirItemKind::ForeignMod{..} |
                                HirItemKind::ExternCrate(_) |
                                HirItemKind::Macro(..) |
                                HirItemKind::TyAlias(..) |
                                HirItemKind::OpaqueTy(_) |
                                HirItemKind::TraitAlias(..) => (),
                                // simple items
                                HirItemKind::Const(..) => {
                                    // todo?
                                }
                                HirItemKind::Static(..) => {
                                    // todo?
                                }
                                HirItemKind::Fn(..) => {
                                    let local_id = item.owner_id.def_id;
                                    let item_path = hir.def_path(local_id).to_string_no_crate_verbose();

                                    let path = ItemPath::new_value(item_path);
                                    let kind = ItemKind::new_function();

                                    items.add_item(vm,kind,path,local_id);
                                }
                                HirItemKind::Struct(..) |
                                HirItemKind::Enum(..) |
                                HirItemKind::Union(..) => {
                                    let local_id = item.owner_id.def_id;
                                    let item_path = hir.def_path(local_id).to_string_no_crate_verbose();

                                    let path = ItemPath::new_type(item_path);
                                    let kind = ItemKind::new_adt();

                                    let item_id = items.add_item(vm,kind,path,local_id);
                                    adt_items.push(item_id);
                                }
                                // todo impls
                                HirItemKind::Trait(_is_auto,_safety,_generics,_bounds,child_items) => {
                                    // trait item
                                    let (trait_item,trait_path) = {
                                        let local_id = item.owner_id.def_id;
                                        let item_path = hir.def_path(local_id).to_string_no_crate_verbose();
    
                                        let path = ItemPath::new_type(item_path.clone());
                                        let kind = ItemKind::new_trait();
    
                                        let item_id = items.add_item(vm,kind,path,local_id);
                                        (item_id,item_path)
                                    };

                                    for item in child_items {
                                        let local_id = item.id.owner_id.def_id;

                                        let AssocItemKind::Fn{..} = item.kind else {
                                            panic!("unsupported associated item");
                                        };

                                        let item_path = format!("{}::{}",trait_path,item.ident);

                                        let path = ItemPath::new_value(item_path);
                                        let kind = ItemKind::new_function_with_trait(trait_item,item.ident.as_str().to_owned());

                                        items.add_item(vm,kind,path,local_id);
                                    }
                                }
                                HirItemKind::Impl(impl_info) => {
                                    let impl_id = item.owner_id.def_id;
                                    let self_ty = tcx.type_of(impl_id).skip_binder();

                                    let trait_did = impl_info.of_trait.map(|of_trait| {
                                        of_trait.trait_def_id().unwrap()
                                    });

                                    let trait_path = trait_did.map(|trait_did| {
                                        tcx.def_path(trait_did).to_string_no_crate_verbose()
                                    });
                                    
                                    let mut impl_list: Vec<(String,ItemId)> = Vec::new();

                                    for item in impl_info.items {
                                        let local_id = item.id.owner_id.def_id;

                                        let item_path = if let Some(trait_path) = &trait_path {
                                            format!("<{} as {}>::{}",self_ty,trait_path,item.ident)
                                        } else {
                                            format!("{}::{}",self_ty,item.ident)
                                        };
                                        
                                        let path = ItemPath::new_debug(item_path);

                                        if let AssocItemKind::Fn{..} = item.kind {
                                            let kind = ItemKind::new_function();
    
                                            let item_id = items.add_item(vm,kind,path,local_id);
                                            impl_list.push((item.ident.as_str().to_owned(),item_id));
                                        } else {
                                            // todo
                                        }
                                    }

                                    impl_items.push(ImplItem{
                                        self_ty,
                                        trait_def: trait_did,
                                        children: impl_list
                                    });
                                }
                                /*
                                // traits
                                HirItemKind::Trait(_,_,_,_,items) => {
                                    // create trait item
                                    let trait_item = {
                                        let local_id = item.owner_id.def_id;
                                        let item_path = hir.def_path(local_id).to_string_no_crate_verbose();

                                        let vm_item = vm.items.get(this_crate, &item_path, vm);
                                        println!("todo init trait {}",item_path);
                                        items_global.insert(item_path,(local_id,vm_item));
                                        items_local.insert(local_id.local_def_index,vm_item);
                                        vm_item
                                    };

                                    // TODO improve this
                                    for item in items {
                                        let local_id = item.id.owner_id.def_id;
                                        let item_path = hir.def_path(local_id).to_string_no_crate_verbose();
                                        
                                        let ident = item.ident.as_str().to_owned();

                                        let vm_item = vm.items.get(this_crate, &item_path, vm);
                                        match item.kind {
                                            AssocItemKind::Fn{..} => {
                                                vm_item.init(ItemKind::new_function(Some((ident,trait_item))));
                                            }
                                            _ => panic!("cannot handle kind")
                                        }

                                        // for traits, always add to lookup tables
                                        items_global.insert(item_path,(local_id,vm_item));
                                        items_local.insert(local_id.local_def_index,vm_item);
                                    }
                                }
                                // impls
                                HirItemKind::Impl(impl_info) => {
                                    // TODO: handling impl items the same as normal items is probably way stupid
                                    // todo resolve sane paths for trivial impls, figure out something else for traits
                                    for item in impl_info.items {
                                        let local_id = item.id.owner_id.def_id;
                                        let item_path = hir.def_path(local_id).to_string_no_crate_verbose();

                                        let vm_item = vm.items.get(this_crate, &item_path, vm);
                                        match item.kind {
                                            AssocItemKind::Fn{..} => {
                                                vm_item.init(ItemKind::new_function(None));
                                            }
                                            _ => panic!("cannot handle kind")
                                        }

                                        if let Some(impl_trait) = impl_info.of_trait {
                                            let trait_def = impl_trait.hir_ref_id.owner.def_id.local_def_index;
                                            // todo look up trait globally
                                            let trait_item = items_local.get(&trait_def);
                                            panic!("todo trait impl - {:?}",trait_item);
                                        } else {
                                            // only add refs for trivial impls
                                            items_global.insert(item_path,(local_id,vm_item));
                                            items_local.insert(local_id.local_def_index,vm_item);
                                        }
                                    }
                                }*/

                                _ => panic!("can't handle item kind {:?}",item.kind)
                            }

                        }

                        /*for impl_info in impls {
                            assert!(impl_info.of_trait.is_none());

                            let self_ty = vm.types.type_from_rustc(ty, &ctx);

                            println!("> {:?}",impl_info);
                        }*/

                        let items = vm.set_crate_items(this_crate, items);

                        let ctx = RustCContext{
                            items: &items,
                            tcx,
                            vm
                        };

                        // fill adt fields
                        for item_id in adt_items {
                            let item = items.get(item_id);

                            let adt_def = tcx.adt_def(item.did);
                            let variants = adt_def.variants();

                            assert!(variants.len() == 1);

                            let fields: Vec<_> = variants[rustc_abi::VariantIdx::from_u32(0)].fields.iter().map(|field| {
                                let ty = tcx.type_of(field.did).skip_binder();
                                vm.types.type_from_rustc(ty, &ctx)
                            }).collect();

                            item.set_adt_fields(fields);
                        }

                        // fill impls
                        for impl_item in impl_items {
                            let self_ty = vm.types.type_from_rustc(impl_item.self_ty, &ctx);

                            if let Some(trait_did) = impl_item.trait_def {
                                let trait_item = items.find_by_did(trait_did).expect("todo non-local traits");
                                trait_item.add_trait_impl(self_ty,this_crate, impl_item.children);
                            } else {
                                // todo non trait impls not resolved
                            }
                        }

                        if is_verbose {
                            println!("item aggregation took {:?}",t.elapsed());
                        }

                        loop {
                            let cmd: Box<dyn WorkerCommandDyn> = recv.recv().unwrap();
                            cmd.call(RustCContext{
                                items,
                                tcx,
                                vm
                            });
                        }
                    })
                });
            });
        });

        RustCWorker {
            sender: Mutex::new(sender),
            items: Default::default()
        }
    }

    fn call<T,F>(&self, func: F) -> Arc<WorkerResult<T>> where
        F: Fn(RustCContext)->T + Send + Sync + 'static,
        T: Send + Sync + 'static
    {
        let result = Arc::new(WorkerResult::<T>{
            complete: Barrier::new(2),
            value: Mutex::new(None)
        });

        let cmd = Box::new(WorkerCommand{
            func,
            result: result.clone()
        });

        {
            let sender = self.sender.lock().unwrap();
            sender.send(cmd).unwrap();
        }

        result
    }

    pub fn build_function_ir(&self, item_id: ItemId) {
        let res = self.call(move |ctx| {
            let item = ctx.items.get(item_id);
            let did = item.did;

            let (thir,root) = ctx.tcx.thir_body(did).unwrap();
            let ir = IRFunctionBuilder::build(ctx, did, root, &thir.borrow());
            item.set_ir(ir);
        });
        res.wait();
    }

    pub fn wait_for_setup(&self) -> Arc<WorkerResult<()>> {
        self.call( |_ctx| {})
    }
}
