use std::{
    sync::{Arc, Barrier, Mutex, OnceLock},
    time::Instant, borrow::Borrow,
};

use ahash::AHashMap;
use rustc_hir::def_id::{LocalDefId, DefId};
use rustc_hir::AssocItemKind;
use rustc_hir::ItemKind as HirItemKind;
use rustc_hir::VariantData;
use rustc_middle::ty::{ImplSubject, Ty, TyCtxt};
use rustc_session::config;

use crate::{
    builtins::BuiltinTrait,
    ir::{converter::IRFunctionConverter, IRFunctionBuilder, IRFunction},
    items::{
        path_from_rustc, AdtInfo, AdtKind, AssocValue, BoundKind, CrateId, CrateItems, ExternCrate,
        FunctionAbi, GenericCounts, ItemId, ItemKind, ItemPath, TraitImpl, Item,
    },
    types::{IntSign, IntWidth, Type, TypeKind},
    vm::VM, crate_provider::CrateProvider,
};

/////////////////////////

pub struct RustCWorker<'vm> {
    sender: Mutex<std::sync::mpsc::Sender<Box<dyn WorkerCommandDyn<'vm>>>>,
    items: OnceLock<Arc<RustCItems<'vm>>>
}

pub struct WorkerResult<T> {
    complete: Barrier,
    value: Mutex<Option<T>>,
}

impl<T> WorkerResult<T> {
    pub fn wait(&self) -> T {
        self.complete.wait();
        let mut value = self.value.lock().unwrap();
        value.take().unwrap()
    }
}

struct WorkerCommand<T, F> {
    func: F,
    result: Arc<WorkerResult<T>>,
}

trait WorkerCommandDyn<'vm>: Send + Sync + 'vm {
    fn call(&self, context: &RustCContext<'vm,'_>);
}

impl<'vm, T, F> WorkerCommandDyn<'vm> for WorkerCommand<T, F>
where
    F: Fn(&RustCContext<'vm,'_>) -> T + Send + Sync + 'vm,
    T: Send + Sync + 'vm,
{
    fn call(&self, context: &RustCContext<'vm,'_>) {
        let res = (self.func)(context);
        {
            let mut guard = self.result.value.lock().unwrap();
            *guard = Some(res);
        }
        self.result.complete.wait();
    }
}

//#[derive(Debug)]
struct ImplItem<'tcx, 'vm> {
    did: LocalDefId,
    subject: rustc_middle::ty::ImplSubject<'tcx>,
    assoc_values: Vec<(String, AssocValue<'vm>)>,
    assoc_tys: Vec<(String, LocalDefId)>,
}

pub struct RustCWorkerConfig<'a> {
    pub source_root: &'a str,
    pub extern_crates: Vec<ExternCrate>,
    pub is_core: bool,
    pub save: bool,
}

impl<'vm> RustCWorker<'vm> {
    pub fn new(
        worker_config: RustCWorkerConfig,
        vm: &'vm VM<'vm>,
        this_crate: CrateId,
    ) -> Self where 'vm: 'static {

        let (sender, recv) = std::sync::mpsc::channel::<Box<dyn WorkerCommandDyn>>();

        // fixme?
        let self_profile = if false {
            config::SwitchWithOptPath::Enabled(None)
        } else {
            config::SwitchWithOptPath::Disabled
        };

        let crate_name = if worker_config.is_core {
            Some("core".to_owned())
        } else {
            None
        };

        let config = rustc_interface::Config {
            opts: config::Options {
                crate_name,
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
            input: config::Input::File(worker_config.source_root.into()),
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
                compiler.enter(move |queries| {
                    queries.global_ctxt().unwrap().enter(|tcx| {

                        let ctx = RustCContext::new(vm, tcx, this_crate);

                        loop {
                            let cmd: Box<dyn WorkerCommandDyn> = recv.recv().unwrap();
                            cmd.call(&ctx);
                        }
                    })
                })
            });
        });

        RustCWorker {
            sender: Mutex::new(sender),
            items: Default::default(),
        }
    }

                        /*let t = Instant::now();

                        let mut adt_items = Vec::new();
                        let mut impl_items: Vec<ImplItem> = Vec::new();

                        for item_id in hir_items {
                            let item = hir.item(item_id);
                            match item.kind {
                                // useless to us
                                HirItemKind::Use(..)
                                | HirItemKind::Mod(_)
                                | HirItemKind::ExternCrate(_)
                                | HirItemKind::Macro(..)
                                | HirItemKind::TyAlias(..)
                                | HirItemKind::OpaqueTy(_)
                                | HirItemKind::TraitAlias(..) => (),
                                // simple items
                                HirItemKind::Const(ty, body_id) => {
                                    let local_id = item.owner_id.def_id;
                                    let item_path = path_from_rustc(&hir.def_path(local_id), vm);

                                    let kind = ItemKind::new_const();

                                    items.add_item(vm, kind, item_path, local_id);
                                }
                                HirItemKind::Static(..) => {
                                    // todo?
                                }
                                HirItemKind::Fn(..) => {
                                    let local_id = item.owner_id.def_id;
                                    let item_path = path_from_rustc(&hir.def_path(local_id), vm);

                                    let kind = ItemKind::new_function();

                                    items.add_item(vm, kind, item_path, local_id);
                                }
                                HirItemKind::Struct(variant, _)
                                | HirItemKind::Union(variant, _) => {
                                    let adt_id = {
                                        let local_id = item.owner_id.def_id;
                                        let item_path =
                                            path_from_rustc(&hir.def_path(local_id), vm);

                                        let kind = ItemKind::new_adt();

                                        let item_id = items.add_item(vm, kind, item_path, local_id);
                                        adt_items.push(item_id);
                                        item_id
                                    };

                                    // add ctor
                                    match variant {
                                        VariantData::Struct(..) => (),
                                        VariantData::Tuple(_, _, local_id) => {
                                            let item_path =
                                                path_from_rustc(&hir.def_path(local_id), vm);

                                            let kind = ItemKind::new_function_ctor(adt_id, 0);

                                            items.add_item(vm, kind, item_path, local_id);
                                        }
                                        VariantData::Unit(_, local_id) => {
                                            let item_path =
                                                path_from_rustc(&hir.def_path(local_id), vm);

                                            let kind = ItemKind::new_const_ctor(adt_id, 0);

                                            items.add_item(vm, kind, item_path, local_id);
                                        }
                                    }
                                }
                                HirItemKind::Enum(enum_def, _) => {
                                    let adt_id = {
                                        let local_id = item.owner_id.def_id;
                                        let item_path =
                                            path_from_rustc(&hir.def_path(local_id), vm);

                                        let kind = ItemKind::new_adt();

                                        let item_id = items.add_item(vm, kind, item_path, local_id);
                                        adt_items.push(item_id);
                                        item_id
                                    };

                                    // add ctors
                                    for (index, variant) in enum_def.variants.iter().enumerate() {
                                        match variant.data {
                                            VariantData::Struct(..) => (),
                                            VariantData::Tuple(_, _, local_id) => {
                                                let item_path =
                                                    path_from_rustc(&hir.def_path(local_id), vm);

                                                let kind = ItemKind::new_function_ctor(
                                                    adt_id,
                                                    index as u32,
                                                );

                                                items.add_item(vm, kind, item_path, local_id);
                                            }
                                            VariantData::Unit(_, local_id) => {
                                                let item_path =
                                                    path_from_rustc(&hir.def_path(local_id), vm);

                                                let kind =
                                                    ItemKind::new_const_ctor(adt_id, index as u32);

                                                items.add_item(vm, kind, item_path, local_id);
                                            }
                                        }
                                    }
                                }
                                HirItemKind::ForeignMod {
                                    abi,
                                    items: mod_items,
                                } => {
                                    //let local_id = item.owner_id.def_id;
                                    //let item_path = hir.def_path(local_id).to_string_no_crate_verbose();
                                    for item in mod_items {
                                        // only statics and functions are permitted according to ref
                                        let local_id = item.id.owner_id.def_id;
                                        let item_path =
                                            path_from_rustc(&hir.def_path(local_id), vm);

                                        let item = hir.foreign_item(item.id);

                                        use rustc_hir::ForeignItemKind;
                                        match item.kind {
                                            ForeignItemKind::Fn(..) => {
                                                let kind = if abi
                                                    == rustc_target::spec::abi::Abi::RustIntrinsic
                                                {
                                                    let ident = item.ident.as_str().to_owned();
                                                    let abi = FunctionAbi::RustIntrinsic;
                                                    ItemKind::new_function_extern(abi, ident)
                                                } else {
                                                    ItemKind::new_function()
                                                };

                                                items.add_item(vm, kind, item_path, local_id);
                                            }
                                            ForeignItemKind::Type => {
                                                // opaque types, eww
                                            }
                                            _ => panic!("todo foreign item {:?}", item_path),
                                        }
                                    }
                                }
                                HirItemKind::Trait(
                                    _is_auto,
                                    _safety,
                                    _generics,
                                    _bounds,
                                    child_items,
                                ) => {
                                    // trait item
                                    let trait_item = {
                                        let local_id = item.owner_id.def_id;
                                        let item_path =
                                            path_from_rustc(&hir.def_path(local_id), vm);

                                        let kind = ItemKind::new_trait();

                                        let item_id =
                                            items.add_item(vm, kind, item_path.clone(), local_id);
                                        item_id
                                    };

                                    for item in child_items {
                                        let local_id = item.id.owner_id.def_id;
                                        let item_path =
                                            path_from_rustc(&hir.def_path(local_id), vm);

                                        let ident = item.ident.as_str().to_owned();

                                        match item.kind {
                                            AssocItemKind::Fn { .. } => {
                                                let kind = ItemKind::new_function_virtual(
                                                    trait_item, ident,
                                                );
                                                items.add_item(vm, kind, item_path, local_id);
                                            }
                                            AssocItemKind::Type => {
                                                let kind = ItemKind::new_associated_type(
                                                    trait_item, ident,
                                                );
                                                items.add_item(vm, kind, item_path, local_id);
                                            }
                                            AssocItemKind::Const => {
                                                let kind =
                                                    ItemKind::new_const_virtual(trait_item, ident);
                                                items.add_item(vm, kind, item_path, local_id);
                                            }
                                        }
                                    }
                                }
                                HirItemKind::Impl(impl_info) => {
                                    let impl_id = item.owner_id.def_id;

                                    let subject = tcx.impl_subject(impl_id.into()).skip_binder();

                                    let base_path = match subject {
                                        rustc_middle::ty::ImplSubject::Trait(x) => {
                                            format!("{:?}", x)
                                        }
                                        rustc_middle::ty::ImplSubject::Inherent(x) => {
                                            format!("{:?}", x)
                                        }
                                    };

                                    let mut assoc_values: Vec<(String, AssocValue<'vm>)> =
                                        Vec::new();
                                    let mut assoc_tys: Vec<(String, LocalDefId)> = Vec::new();

                                    for item in impl_info.items {
                                        let local_id = item.id.owner_id.def_id;

                                        let item_name = item.ident.as_str().to_owned();

                                        match item.kind {
                                            AssocItemKind::Fn { .. } => {
                                                let item_path = ItemPath::new_debug(
                                                    &format!("{}::{}", base_path, item.ident),
                                                    vm,
                                                );

                                                let kind = ItemKind::new_function();
                                                let item_id =
                                                    items.add_item(vm, kind, item_path, local_id);

                                                assoc_values
                                                    .push((item_name, AssocValue::Item(item_id)));
                                            }
                                            AssocItemKind::Const => {
                                                let item_path = ItemPath::new_debug(
                                                    &format!("{}::{}", base_path, item.ident),
                                                    vm,
                                                );

                                                let kind = ItemKind::new_const();
                                                let item_id =
                                                    items.add_item(vm, kind, item_path, local_id);

                                                assoc_values
                                                    .push((item_name, AssocValue::Item(item_id)));
                                            }
                                            AssocItemKind::Type => {
                                                assoc_tys.push((item_name, local_id));
                                            }
                                        }
                                    }

                                    impl_items.push(ImplItem {
                                        did: impl_id,
                                        subject,
                                        assoc_values,
                                        assoc_tys,
                                    });
                                }
                                _ => panic!("can't handle item kind {:?}", item.kind),
                            }
                        }

                        let items = vm.set_crate_items(this_crate, items);

                        let ctx = RustCContext {
                            items: &items,
                            tcx,
                            vm,
                        };

                        // fill adt fields
                        for item_id in adt_items {
                            let item = items.get(item_id);

                            let adt_def = tcx.adt_def(item.did.unwrap());

                            let variant_fields = adt_def
                                .variants()
                                .iter()
                                .map(|variant| {
                                    variant
                                        .fields
                                        .iter()
                                        .map(|field| {
                                            let ty = tcx.type_of(field.did).skip_binder();
                                            vm.types.type_from_rustc(ty, &ctx)
                                        })
                                        .collect()
                                })
                                .collect();

                            let kind = if adt_def.is_enum() {
                                let kind = TypeKind::Int(IntWidth::I32, IntSign::Unsigned);
                                AdtKind::EnumWithDiscriminant(vm.types.intern(kind, vm))
                            } else {
                                AdtKind::Struct
                            };

                            item.set_adt_info(AdtInfo {
                                variant_fields,
                                kind,
                            });
                        }

                        // fill impls
                        for impl_item in impl_items {
                            match impl_item.subject {
                                ImplSubject::Inherent(ty) => {
                                    let ty = vm.types.type_from_rustc(ty, &ctx);
                                    ty.add_impl(this_crate, impl_item.assoc_values);
                                    assert!(impl_item.assoc_tys.len() == 0);
                                }
                                ImplSubject::Trait(trait_ref) => {
                                    let assoc_tys: AHashMap<_, _> = impl_item
                                        .assoc_tys
                                        .into_iter()
                                        .map(|(name, local_id)| {
                                            let ty = tcx.type_of(local_id).skip_binder();
                                            let ty = vm.types.type_from_rustc(ty, &ctx);
                                            (name, ty)
                                        })
                                        .collect();

                                    let trait_did = trait_ref.def_id;

                                    // Convert bounds.
                                    let mut bounds: Vec<BoundKind> = Vec::new();
                                    let predicates = tcx.predicates_of(impl_item.did);
                                    for (p, _) in predicates.predicates {
                                        let p = p.kind().skip_binder();
                                        if let rustc_middle::ty::PredicateKind::Clause(p) = p {
                                            if let rustc_middle::ty::Clause::Trait(p) = p {
                                                if p.polarity
                                                    == rustc_middle::ty::ImplPolarity::Positive
                                                {
                                                    let trait_bound = vm.types.def_from_rustc(
                                                        p.trait_ref.def_id,
                                                        p.trait_ref.substs,
                                                        &ctx,
                                                    );
                                                    bounds.push(BoundKind::Trait(trait_bound));
                                                }
                                            } else if let rustc_middle::ty::Clause::Projection(p) =
                                                p
                                            {
                                                let assoc_ty = vm.types.def_from_rustc(
                                                    p.projection_ty.def_id,
                                                    p.projection_ty.substs,
                                                    &ctx,
                                                );
                                                if let rustc_middle::ty::TermKind::Ty(ty) =
                                                    p.term.unpack()
                                                {
                                                    let eq_ty = vm.types.type_from_rustc(ty, &ctx);
                                                    bounds.push(BoundKind::Projection(
                                                        assoc_ty, eq_ty,
                                                    ));
                                                }
                                                // TODO CONSTS
                                            }
                                        }
                                    }

                                    // Build generics summary.
                                    let rustc_generics = tcx.generics_of(impl_item.did);

                                    let mut generics = GenericCounts::default();

                                    for gp in &rustc_generics.params {
                                        use rustc_middle::ty::GenericParamDefKind;
                                        match gp.kind {
                                            GenericParamDefKind::Lifetime => {
                                                generics.lifetimes += 1
                                            }
                                            GenericParamDefKind::Type { .. } => generics.types += 1,
                                            GenericParamDefKind::Const { .. } => {
                                                generics.consts += 1
                                            }
                                        }
                                    }

                                    let trait_item = if let Some(trait_item) =
                                        items.find_by_did(trait_did)
                                    {
                                        trait_item
                                    } else {
                                        let trait_path =
                                            path_from_rustc(&tcx.def_path(trait_did), vm);
                                        let trait_crate_id =
                                            ctx.items.find_crate_id(tcx, trait_did.krate);
                                        let trait_crate_items = vm.get_crate_items(trait_crate_id);

                                        trait_crate_items
                                            .find_by_path(&trait_path)
                                            .expect("couldn't find trait")
                                    };

                                    let for_types =
                                        vm.types.subs_from_rustc(trait_ref.substs, &ctx);

                                    trait_item.add_trait_impl(TraitImpl {
                                        crate_id: this_crate,
                                        for_types,
                                        assoc_values: impl_item.assoc_values.into_iter().collect(),
                                        assoc_tys,
                                        bounds,
                                        generics,
                                    });
                                }
                            }
                        }

                        if worker_config.is_core {
                            let lang_items = tcx.lang_items();
                            {
                                let lang_trait = lang_items.sized_trait().unwrap();
                                let lang_trait = items.find_by_did(lang_trait).unwrap();
                                lang_trait.trait_set_builtin(BuiltinTrait::Sized);
                            }
                            {
                                let lang_trait = lang_items.tuple_trait().unwrap();
                                let lang_trait = items.find_by_did(lang_trait).unwrap();
                                lang_trait.trait_set_builtin(BuiltinTrait::Tuple);
                            }
                            {
                                let lang_trait = lang_items.discriminant_kind_trait().unwrap();
                                let lang_trait = items.find_by_did(lang_trait).unwrap();
                                lang_trait.trait_set_builtin(BuiltinTrait::DiscriminantKind);
                            }
                            {
                                let lang_trait = lang_items.fn_once_trait().unwrap();
                                let lang_trait = items.find_by_did(lang_trait).unwrap();
                                lang_trait.trait_set_builtin(BuiltinTrait::FnOnce);
                            }
                            {
                                let lang_trait = lang_items.fn_mut_trait().unwrap();
                                let lang_trait = items.find_by_did(lang_trait).unwrap();
                                lang_trait.trait_set_builtin(BuiltinTrait::FnMut);
                            }
                            {
                                let lang_trait = lang_items.fn_trait().unwrap();
                                let lang_trait = items.find_by_did(lang_trait).unwrap();
                                lang_trait.trait_set_builtin(BuiltinTrait::Fn);
                            }
                        }

                        if is_verbose {
                            println!("item aggregation took {:?}", t.elapsed());
                            println!("n = {}", items.count());
                        }

                        if worker_config.save {
                            println!("generating ir for all bodies");
                            for item in ctx.items.all() {
                                //if item.is_meh() {
                                let did = item.did.unwrap();
                                let has_body = hir.maybe_body_owned_by(did).is_some();
                                if has_body {
                                    println!("go {:?}", did);
                                    //let (thir, root) = ctx.tcx.thir_body(did).unwrap();
                                    //let ir = IRFunctionBuilder::build(ctx.clone(), did, root, &thir.borrow());
                                    println!("done");
                                }
                                //}
                            }
                        }

                        loop {
                            let cmd: Box<dyn WorkerCommandDyn> = recv.recv().unwrap();
                            cmd.call(RustCContext { items, tcx, vm });
                        }*/


    fn call<T, F>(&self, func: F) -> Arc<WorkerResult<T>>
    where
        F: Fn(&RustCContext<'vm,'_>) -> T + Send + Sync + 'vm,
        T: Send + Sync + 'vm,
    {
        let result = Arc::new(WorkerResult::<T> {
            complete: Barrier::new(2),
            value: Mutex::new(None),
        });

        let cmd = Box::new(WorkerCommand {
            func,
            result: result.clone(),
        });

        {
            let sender = self.sender.lock().unwrap();
            sender.send(cmd).unwrap();
        }

        result
    }

    fn items(&self) -> &RustCItems<'vm> {
        self.items.get_or_init(|| {
            let res = self.call(|ctx| {
                ctx.items.clone()
            });
            res.wait()
        })
    }

    /*fn init_item(&self, item_id: ItemId, def_id: LocalDefId) -> &'vm Item<'vm> {
        let res = self.call(move |ctx| {
            let hir = ctx.tcx.hir();

            let item_source = hir.expect_item(def_id);

            let kind = match item_source.kind {
                HirItemKind::Fn(..) => {
                    ItemKind::new_function()
                }
                _ => panic!("todo init {:?}",item_source)
            };

            let item = Item::new(self.vm, self.crate_id, item_id, path, kind);
        });
        res.wait()
    }*/

    /*pub fn build_function_ir(&self, item_id: ItemId) {
        let res = self.call(move |ctx| {
            let item = ctx.items.get(item_id);
            let did = item.did.unwrap();

            let hir = ctx.tcx.hir();

            let body_id = hir.body_owned_by(did);
            let body = hir.body(body_id);

            let types = ctx.tcx.typeck(did);

            let is_constant = !item.is_function();

            let ir = IRFunctionConverter::run(ctx, did, body, types, is_constant);
            item.set_ir(ir);
        });
        res.wait();
    }

    pub fn wait_for_setup(&self) -> Arc<WorkerResult<()>> {
        self.call(|_ctx| {})
    }*/
}

impl<'vm> CrateProvider<'vm> for RustCWorker<'vm> {
    fn item_by_id(&self, item_id: ItemId) -> &'vm Item<'vm> {
        let items = self.items();

        items.items[item_id.index()].item
    }

    fn item_by_path(&self, path: &ItemPath) -> Option<&'vm Item<'vm>> {
        let items = self.items();

        let item_id = items.map_paths.get(path);

        item_id.map(|item_id| {
            items.items[item_id.index()].item
        })
    }

    fn build_ir(&self, item_id: ItemId) -> Arc<IRFunction<'vm>> {
        let items = self.items();

        let item_info = &items.items[item_id.index()];
        let did = item_info.did;
        let is_constant = !item_info.item.is_function();

        let res = self.call(move |ctx| {

            let hir = ctx.tcx.hir();

            let body_id = hir.body_owned_by(did);
            let body = hir.body(body_id);

            let types = ctx.tcx.typeck(did);

            let ir = IRFunctionConverter::run(ctx, did, body, types, is_constant);
            Arc::new(ir)
        });
        res.wait()
    }
}

pub struct RustCContext<'vm, 'tcx> {
    pub vm: &'vm VM<'vm>,
    pub tcx: TyCtxt<'tcx>,

    items: Arc<RustCItems<'vm>>
}

impl<'vm, 'tcx> RustCContext<'vm, 'tcx> {
    pub fn new(vm: &'vm VM<'vm>, tcx: TyCtxt<'tcx>, crate_id: CrateId) -> Self {

        let mut items = RustCItems::default();

        let hir = tcx.hir();

        let t = Instant::now();
        let hir_items = hir.items();
        if vm.is_verbose {
            println!("rustc hir items took {:?}", t.elapsed());
        }

        for item_id in hir_items {
            let item = hir.item(item_id);
            match item.kind {
                // not important
                HirItemKind::Use(..)
                | HirItemKind::Mod(_)
                | HirItemKind::ExternCrate(_)
                | HirItemKind::Macro(..)
                | HirItemKind::TyAlias(..)
                | HirItemKind::OpaqueTy(_)
                | HirItemKind::TraitAlias(..) => (),
                // simple items, add to the index
                HirItemKind::Fn(..) => {
                    let local_id = item.owner_id.def_id;
                    let item_path = path_from_rustc(&hir.def_path(local_id), vm);
                    let kind = ItemKind::new_function();

                    items.index_item(kind,item_path,local_id,vm,crate_id);
                }
                _ => panic!("todo item {:?}",item.kind)
            }
        }

        RustCContext {
            vm,
            tcx,
            items: Arc::new(items)
        }
    }

    pub fn type_from_rustc(&self, ty: Ty<'tcx>) -> Type<'vm> {
        self.vm.types.type_from_rustc(ty, self)
    }

    pub fn item_by_did(&self, did: DefId) -> Option<&'vm Item<'vm>> {
        if did.krate == rustc_hir::def_id::LOCAL_CRATE {
            let local_did = rustc_hir::def_id::LocalDefId {
                local_def_index: did.index,
            };
            self.items.map_defs
                .get(&local_did)
                .map(|item_id| self.items.items[item_id.index()].item)
        } else {
            None
        }
    }

    pub fn find_crate_id(
        &self,
        crate_num: rustc_span::def_id::CrateNum,
    ) -> CrateId {
        todo!()
        /*let mut cache = self.extern_crate_id_cache.lock().unwrap();

        *cache.entry(crate_num).or_insert_with(|| {
            let crate_name = tcx.crate_name(crate_num);
            for entry in &self.extern_crate_list {
                if entry.name == crate_name.as_str() {
                    return entry.id;
                }
            }
            panic!(
                "lookup for crate failed: {} {}",
                crate_num,
                crate_name.as_str()
            );
        })*/
    }
}

#[derive(Default)]
struct RustCItems<'vm> {
    items: Vec<RustCItem<'vm>>,
    map_paths: AHashMap<ItemPath<'vm>,ItemId>,
    map_defs: AHashMap<LocalDefId,ItemId>
}

struct RustCItem<'vm> {
    did: LocalDefId,
    item: &'vm Item<'vm>
}

impl<'vm> RustCItems<'vm> {
    fn index_item(&mut self, kind: ItemKind<'vm>, path: ItemPath<'vm>, did: LocalDefId, vm: &'vm VM<'vm>, crate_id: CrateId) -> ItemId {
        let item_id = ItemId::new(self.items.len() as u32);

        let item = vm.alloc_item(Item::new(vm, crate_id, item_id, path, kind));

        self.items.push(RustCItem{
            did,
            item
        });

        if item.path.can_lookup() {
            let old = self.map_paths.insert(item.path.clone(), item_id);
            assert!(old.is_none());
        }

        {
            let old = self.map_defs.insert(did, item_id);
            assert!(old.is_none());
        }

        item_id
    }
}
