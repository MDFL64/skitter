use std::{sync::{Mutex, Barrier, Arc}, time::Instant};

use rustc_session::config;
use rustc_middle::ty::{TyCtxt, Ty, ImplSubject};
use rustc_hir::ItemKind as HirItemKind;
use rustc_hir::AssocItemKind;
use rustc_hir::def_id::LocalDefId;

use crate::{ir::{IRFunctionBuilder}, vm::{VM}, types::{Type, IntWidth, IntSign, TypeKind}, items::{CrateId, CrateItems, ItemKind, ItemPath, ItemId, ExternCrate, AdtInfo}};

/////////////////////////

pub struct RustCWorker<'vm> {
    sender: Mutex<std::sync::mpsc::Sender<Box<dyn WorkerCommandDyn>>>,
    pub items: Option<&'vm CrateItems<'vm>>,
    //crate_id: CrateId
}

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
    subject: rustc_middle::ty::ImplSubject<'tcx>,
    children_fn: Vec<(String,ItemId)>,
    children_ty: Vec<(String,LocalDefId)>,
}

impl<'vm> RustCWorker<'vm> {
    pub fn new<'s>(source_root: &str, extern_crates: Vec<ExternCrate>, scope: &'s std::thread::Scope<'s,'vm>, vm: &'vm VM<'vm>, this_crate: CrateId) -> Self {

        let is_verbose = vm.is_verbose;

        let (sender,recv) =
            std::sync::mpsc::channel::<Box<dyn WorkerCommandDyn>>();

        // fixme?
        let self_profile = if false { config::SwitchWithOptPath::Enabled(None) } else { config::SwitchWithOptPath::Disabled };

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
        
        scope.spawn(move || {
            rustc_interface::run_compiler(config, |compiler| {
                compiler.enter(move |queries| {
                    queries.global_ctxt().unwrap().enter(|tcx| {
                        
                        let mut items = CrateItems::new(this_crate,extern_crates);
                        
                        let hir = tcx.hir();

                        let t = Instant::now();
                        let hir_items = hir.items();
                        if is_verbose {
                            println!("rustc hir items took {:?}",t.elapsed());
                        }

                        let t = Instant::now();

                        let mut adt_items = Vec::new();
                        let mut impl_items: Vec<ImplItem> = Vec::new();

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

                                        let item_path = format!("{}::{}",trait_path,item.ident);

                                        match item.kind {
                                            AssocItemKind::Fn{..} => {
                                                let path = ItemPath::new_value(item_path);
                                                let kind = ItemKind::new_function_with_trait(trait_item,item.ident.as_str().to_owned());
                                                items.add_item(vm,kind,path,local_id);
                                            }
                                            AssocItemKind::Type => {
                                                let path = ItemPath::new_type(item_path);
                                                let kind = ItemKind::new_associated_type(trait_item, item.ident.as_str().to_owned());
                                                items.add_item(vm,kind,path,local_id);
                                            }
                                            _ => () // constants unhandled
                                        }
                                    }
                                }
                                HirItemKind::Impl(impl_info) => {
                                    let impl_id = item.owner_id.def_id;

                                    let subject = tcx.impl_subject(impl_id.into()).skip_binder();

                                    let base_path = match subject {
                                        rustc_middle::ty::ImplSubject::Trait(x) => format!("{:?}",x),
                                        rustc_middle::ty::ImplSubject::Inherent(x) => format!("{:?}",x),
                                    };
                                    
                                    let mut impl_list_fn: Vec<(String,ItemId)> = Vec::new();
                                    let mut impl_list_ty: Vec<(String,LocalDefId)> = Vec::new();

                                    for item in impl_info.items {
                                        let local_id = item.id.owner_id.def_id;

                                        let item_path = format!("{}::{}",base_path,item.ident);
                                        
                                        let path = ItemPath::new_debug(item_path);

                                        match item.kind {
                                            AssocItemKind::Fn{..} => {
                                                let kind = ItemKind::new_function();
                                                let item_id = items.add_item(vm,kind,path,local_id);
                                                impl_list_fn.push((item.ident.as_str().to_owned(),item_id));
                                            }
                                            AssocItemKind::Type => {
                                                //let kind = ItemKind::new_associated_type();
                                                //let item_id = items.add_item(vm,kind,path,local_id);
                                                impl_list_ty.push((item.ident.as_str().to_owned(),local_id));
                                            }
                                            _ => () // constants unhandled
                                        }
                                    }
                                    
                                    impl_items.push(ImplItem{
                                        subject,
                                        children_fn: impl_list_fn,
                                        children_ty: impl_list_ty
                                    });
                                }
                                _ => panic!("can't handle item kind {:?}",item.kind)
                            }

                        }

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
                            let is_enum = adt_def.is_enum();

                            let variant_fields = adt_def.variants().iter().map(|variant| {
                                variant.fields.iter().map(|field| {
                                    let ty = tcx.type_of(field.did).skip_binder();
                                    vm.types.type_from_rustc(ty, &ctx)
                                }).collect()
                            }).collect();

                            let discriminator_ty = if adt_def.is_enum() {
                                let kind = TypeKind::Int(IntWidth::I32,IntSign::Unsigned);
                                Some(vm.types.intern(kind, vm))
                            } else {
                                None
                            };
        
                            item.set_adt_info(AdtInfo{
                                variant_fields,
                                discriminator_ty
                            });
                        }

                        // fill impls
                        for impl_item in impl_items {
                            let impl_assoc_tys: Vec<_> = impl_item.children_ty.into_iter().map(|(name,local_id)| {
                                let ty = tcx.type_of(local_id).skip_binder();
                                let ty = vm.types.type_from_rustc(ty, &ctx);
                                (name,ty)
                            }).collect();

                            match impl_item.subject {
                                ImplSubject::Inherent(ty) => {
                                    let ty = vm.types.type_from_rustc(ty, &ctx);
                                    ty.add_impl(this_crate,impl_item.children_fn,impl_assoc_tys);
                                }
                                ImplSubject::Trait(trait_ref) => {
                                    let trait_did = trait_ref.def_id;

                                    let trait_item = if let Some(trait_item) = items.find_by_did(trait_did) {
                                        trait_item
                                    } else {
                                        let trait_path = ItemPath::new_type(tcx.def_path(trait_did).to_string_no_crate_verbose());
                                        let trait_crate_id = ctx.items.find_crate_id(tcx, trait_did.krate);
                                        let trait_crate_items = vm.get_crate_items(trait_crate_id);
    
                                        trait_crate_items.find_by_path(&trait_path).expect("couldn't find trait")
                                    };

                                    // TODO convert all the subs and use them in lookups instead?
                                    let impl_for = vm.types.subs_from_rustc(trait_ref.substs, &ctx);

                                    trait_item.add_trait_impl(impl_for,this_crate,impl_item.children_fn,impl_assoc_tys);
                                }
                            }
                        }

                        if is_verbose {
                            println!("item aggregation took {:?}",t.elapsed());
                            println!("n = {}",items.count());
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
