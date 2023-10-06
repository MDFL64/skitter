use std::{
    path::PathBuf,
    rc::Rc,
    str::FromStr,
    sync::{Arc, Barrier, Mutex, OnceLock},
    time::Instant,
};

use ahash::AHashMap;
use rustc_hir::def_id::{DefId, LocalDefId};
use rustc_hir::AssocItemKind;
use rustc_hir::ItemKind as HirItemKind;
use rustc_hir::VariantData;
use rustc_middle::hir::map::Map as HirMap;
use rustc_middle::ty::{ImplSubject, Ty, TyCtxt};
use rustc_session::config;

use crate::{
    builtins::BuiltinTrait,
    crate_provider::{CrateProvider, TraitImplResult},
    impls::{ImplBounds, ImplTable, ImplTableSimple},
    ir::{converter::IRFunctionConverter, IRFunction},
    items::{
        ident_from_rustc, path_from_rustc, AdtInfo, AdtKind, AssocValue, BoundKind, CrateId,
        EnumInfo, ExternCrate, FunctionAbi, GenericCounts, Item, ItemId, ItemKind, ItemPath,
        TraitImpl,
    },
    lazy_collections::{LazyArray, LazyTable},
    persist::{Persist, PersistWriteContext, PersistWriter},
    persist_header::{persist_header_write, PersistCrateHeader},
    types::{IntSign, IntWidth, ItemWithSubs, Sub, SubList, Type, TypeKind},
    vm::VM,
    CratePath,
};

/////////////////////////

pub struct RustCWorker<'vm> {
    sender: Mutex<std::sync::mpsc::Sender<Box<dyn WorkerCommandDyn<'vm>>>>,
    items: OnceLock<Arc<RustCItems<'vm>>>,
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
    fn call(&self, context: &RustCContext<'vm, '_>);
}

impl<'vm, T, F> WorkerCommandDyn<'vm> for WorkerCommand<T, F>
where
    F: Fn(&RustCContext<'vm, '_>) -> T + Send + Sync + 'vm,
    T: Send + Sync + 'vm,
{
    fn call(&self, context: &RustCContext<'vm, '_>) {
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

pub struct RustCWorkerConfig {
    pub crate_path: CratePath,
    pub extern_crates: Vec<ExternCrate>,
    pub save_file: bool,
}

impl<'vm> RustCWorker<'vm> {
    pub fn new(worker_config: RustCWorkerConfig, vm: &'vm VM<'vm>, this_crate: CrateId) -> Self
    where
        'vm: 'static,
    {
        let (sender, recv) = std::sync::mpsc::channel::<Box<dyn WorkerCommandDyn>>();

        // fixme?
        let self_profile = if false {
            config::SwitchWithOptPath::Enabled(None)
        } else {
            config::SwitchWithOptPath::Disabled
        };

        let config = rustc_interface::Config {
            opts: config::Options {
                crate_name: Some(worker_config.crate_path.name.clone()),
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
            input: config::Input::File(worker_config.crate_path.source_path()),
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
                        let ctx = RustCContext::new(vm, tcx, this_crate, worker_config);

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

    fn call<T, F>(&self, func: F) -> Arc<WorkerResult<T>>
    where
        F: Fn(&RustCContext<'vm, '_>) -> T + Send + Sync + 'vm,
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
            let res = self.call(|ctx| ctx.items.clone());
            res.wait()
        })
    }
}

impl<'vm> CrateProvider<'vm> for RustCWorker<'vm> {
    fn item_by_id(&self, item_id: ItemId) -> &'vm Item<'vm> {
        let items = self.items();

        items.items[item_id.index()].item
    }

    fn item_by_path(&self, path: &ItemPath) -> Option<&'vm Item<'vm>> {
        let items = self.items();

        let item_id = items.map_paths.get(path);

        item_id.map(|item_id| items.items[item_id.index()].item)
    }

    fn build_ir(&self, item_id: ItemId) -> Arc<IRFunction<'vm>> {
        let items = self.items();

        let item_info = &items.items[item_id.index()];
        let did = item_info.did;
        let is_constant = !item_info.item.is_function();

        let res = self
            .call(move |ctx| build_ir(ctx, did, is_constant).expect("build_ir: no ir available"));
        res.wait()
    }

    fn build_adt(&self, id: ItemId) -> AdtInfo<'vm> {
        panic!("rustc worker should create all adt info eagerly");
    }

    fn trait_impl(
        &self,
        trait_item: &Item<'vm>,
        for_tys: &SubList<'vm>,
    ) -> Option<TraitImplResult<'vm>> {
        let items = self.items();
        let impls = items.impls.get().expect("no impls available");
        impls.find_trait(trait_item, for_tys)
    }

    fn inherent_impl(&self, full_key: &str, ty: Type<'vm>) -> Option<AssocValue<'vm>> {
        let items = self.items();
        let impls = items.impls.get().expect("no impls available");
        impls.find_inherent(full_key, ty)
    }
}

fn build_ir<'vm, 'tcx>(
    ctx: &RustCContext<'vm, 'tcx>,
    did: LocalDefId,
    is_constant: bool,
) -> Option<Arc<IRFunction<'vm>>> {
    let hir = ctx.tcx.hir();

    if let Some(body_id) = hir.maybe_body_owned_by(did) {
        let body = hir.body(body_id);

        let types = ctx.tcx.typeck(did);

        Some(Arc::new(IRFunctionConverter::run(
            ctx,
            did,
            body,
            types,
            is_constant,
        )))
    } else {
        None
    }
}

pub struct RustCContext<'vm, 'tcx> {
    pub vm: &'vm VM<'vm>,
    pub tcx: TyCtxt<'tcx>,

    items: Arc<RustCItems<'vm>>,

    extern_crates: Vec<ExternCrate>,
    extern_crate_id_cache: Mutex<AHashMap<rustc_span::def_id::CrateNum, CrateId>>,
}

impl<'vm, 'tcx> RustCContext<'vm, 'tcx> {
    pub fn new(
        vm: &'vm VM<'vm>,
        tcx: TyCtxt<'tcx>,
        this_crate: CrateId,
        worker_config: RustCWorkerConfig,
    ) -> Self {
        let mut items = RustCItems::new(this_crate);

        let hir = tcx.hir();

        let t = Instant::now();
        let hir_items = hir.items();
        if vm.is_verbose {
            println!("rustc hir items took {:?}", t.elapsed());
        }

        // ADT fields are deferred until all items are aggregated
        let mut adt_ids = Vec::new();
        let mut impl_items = Vec::new();

        for item_id in hir_items {
            let item = hir.item(item_id);
            let local_id = item.owner_id.def_id;

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
                    let item_path = path_from_rustc(&hir.def_path(local_id), vm);
                    let kind = ItemKind::new_function();

                    items.index_item(kind, item_path, local_id, vm);
                }
                HirItemKind::Const(..) => {
                    let item_path = path_from_rustc(&hir.def_path(local_id), vm);
                    let kind = ItemKind::new_const();

                    items.index_item(kind, item_path, local_id, vm);
                }
                HirItemKind::Static(..) => {
                    // todo
                }
                HirItemKind::Struct(variant, _) | HirItemKind::Union(variant, _) => {
                    let adt_id = {
                        let item_path = path_from_rustc(&hir.def_path(local_id), vm);
                        let kind = ItemKind::new_adt();

                        items.index_item(kind, item_path, local_id, vm)
                    };
                    adt_ids.push(adt_id);

                    // add ctor
                    if let Some((kind, item_path, local_id)) =
                        Self::ctor_params(&variant, adt_id, 0, vm, &hir)
                    {
                        items.index_item(kind, item_path, local_id, vm);
                    }
                }
                HirItemKind::Enum(enum_def, _) => {
                    let adt_id = {
                        let item_path = path_from_rustc(&hir.def_path(local_id), vm);
                        let kind = ItemKind::new_adt();

                        items.index_item(kind, item_path, local_id, vm)
                    };
                    adt_ids.push(adt_id);

                    // add ctors
                    for (index, variant) in enum_def.variants.iter().enumerate() {
                        if let Some((kind, item_path, local_id)) =
                            Self::ctor_params(&variant.data, adt_id, index as u32, vm, &hir)
                        {
                            items.index_item(kind, item_path, local_id, vm);
                        }
                    }
                }
                // non-trivial items which contain other items
                HirItemKind::Trait(_is_auto, _safety, _generics, _bounds, child_items) => {
                    // trait item
                    let trait_id = {
                        let local_id = item.owner_id.def_id;
                        let item_path = path_from_rustc(&hir.def_path(local_id), vm);

                        let kind = ItemKind::new_trait();

                        items.index_item(kind, item_path, local_id, vm)
                    };

                    let mut assoc_value_map = AHashMap::new();
                    let mut next_id = 0;
                    for item in child_items {
                        let local_id = item.id.owner_id.def_id;
                        let item_path = path_from_rustc(&hir.def_path(local_id), vm);

                        let item_ident = ident_from_rustc(&hir.def_path(local_id), vm);
                        let id_in_trait = next_id;
                        next_id += 1;
                        assoc_value_map.insert(item_ident, id_in_trait);

                        //let ident = item.ident.as_str().to_owned();

                        let kind = match item.kind {
                            AssocItemKind::Fn { .. } => {
                                ItemKind::new_function_virtual(trait_id, id_in_trait)
                            }
                            AssocItemKind::Type => {
                                ItemKind::new_associated_type(trait_id, id_in_trait)
                            }
                            AssocItemKind::Const => {
                                ItemKind::new_const_virtual(trait_id, id_in_trait)
                            }
                        };
                        items.index_item(kind, item_path, local_id, vm);
                    }

                    let trait_item = items.items[trait_id.index()].item;
                    trait_item.trait_set_assoc_value_map(assoc_value_map);
                }
                HirItemKind::Impl(impl_info) => {
                    let impl_id = item.owner_id.def_id;

                    let subject = tcx.impl_subject(impl_id.into()).skip_binder();

                    let base_path = match subject {
                        rustc_middle::ty::ImplSubject::Trait(x) => format!("{:?}", x),
                        rustc_middle::ty::ImplSubject::Inherent(x) => format!("{:?}", x),
                    };

                    let mut assoc_values: Vec<(String, AssocValue<'vm>)> = Vec::new();
                    let mut assoc_tys: Vec<(String, LocalDefId)> = Vec::new();

                    for item in impl_info.items {
                        let local_id = item.id.owner_id.def_id;

                        let item_name = item.ident.as_str().to_owned();

                        match item.kind {
                            AssocItemKind::Fn { .. } => {
                                let item_path = ItemPath::for_debug(
                                    vm.alloc_path(&format!("{}::{}", base_path, item.ident)),
                                );

                                let kind = ItemKind::new_function();
                                let item_id = items.index_item(kind, item_path, local_id, vm);

                                assoc_values.push((item_name, AssocValue::Item(item_id)));
                            }
                            AssocItemKind::Const => {
                                let item_path = ItemPath::for_debug(
                                    vm.alloc_path(&format!("{}::{}", base_path, item.ident)),
                                );

                                let kind = ItemKind::new_const();
                                let item_id = items.index_item(kind, item_path, local_id, vm);

                                assoc_values.push((item_name, AssocValue::Item(item_id)));
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
                HirItemKind::ForeignMod {
                    abi,
                    items: mod_items,
                } => {
                    //let local_id = item.owner_id.def_id;
                    //let item_path = hir.def_path(local_id).to_string_no_crate_verbose();
                    for item in mod_items {
                        // only statics and functions are permitted according to ref
                        let local_id = item.id.owner_id.def_id;
                        let item_path = path_from_rustc(&hir.def_path(local_id), vm);

                        let item = hir.foreign_item(item.id);

                        use rustc_hir::ForeignItemKind;
                        match item.kind {
                            ForeignItemKind::Fn(..) => {
                                let kind = if abi == rustc_target::spec::abi::Abi::RustIntrinsic {
                                    let ident = item.ident.as_str().to_owned();
                                    let abi = FunctionAbi::RustIntrinsic;
                                    ItemKind::new_function_extern(abi, ident)
                                } else {
                                    ItemKind::new_function()
                                };

                                items.index_item(kind, item_path, local_id, vm);
                            }
                            ForeignItemKind::Type => {
                                // opaque types, eww
                            }
                            _ => panic!("todo foreign item {:?}", item_path),
                        }
                    }
                }
                _ => panic!("todo item {:?}", item.kind),
            }
        }

        let ctx = RustCContext {
            vm,
            tcx,
            items: Arc::new(items),
            extern_crates: worker_config.extern_crates,
            extern_crate_id_cache: Default::default(),
        };

        // fill ADT fields
        for adt_id in adt_ids {
            let item_info = &ctx.items.items[adt_id.index()];

            let adt_def = tcx.adt_def(item_info.did);

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
                // unless specified, the external discriminant MUST be an isize,
                // rustc makes some annoying assumptions when generating derive code
                AdtKind::Enum(EnumInfo {
                    discriminant_internal: vm.common_types().u32,
                    discriminant_external: vm.common_types().isize,
                })
            } else if adt_def.is_union() {
                AdtKind::Union
            } else {
                AdtKind::Struct
            };

            item_info.item.set_adt_info(AdtInfo {
                variant_fields,
                kind,
            });
        }

        let mut impls = ImplTableSimple::new(this_crate);

        // fill impls
        for impl_item in impl_items {
            // Build bounds.
            let mut bounds: Vec<BoundKind> = Vec::new();
            {
                let predicates = tcx.predicates_of(impl_item.did);
                for (p, _) in predicates.predicates {
                    let p = p.kind().skip_binder();
                    if let rustc_middle::ty::PredicateKind::Clause(p) = p {
                        if let rustc_middle::ty::Clause::Trait(p) = p {
                            if p.polarity == rustc_middle::ty::ImplPolarity::Positive {
                                let trait_bound = vm.types.def_from_rustc(
                                    p.trait_ref.def_id,
                                    p.trait_ref.substs,
                                    &ctx,
                                );
                                bounds.push(BoundKind::Trait(trait_bound));
                            }
                        } else if let rustc_middle::ty::Clause::Projection(p) = p {
                            let assoc_ty = vm.types.def_from_rustc(
                                p.projection_ty.def_id,
                                p.projection_ty.substs,
                                &ctx,
                            );
                            if let rustc_middle::ty::TermKind::Ty(ty) = p.term.unpack() {
                                let eq_ty = vm.types.type_from_rustc(ty, &ctx);
                                bounds.push(BoundKind::Projection(assoc_ty, eq_ty));
                            }
                            // TODO CONSTS
                        }
                    }
                }
            }

            // Build generics summary.
            let mut generic_counts = GenericCounts::default();
            {
                let rustc_generics = tcx.generics_of(impl_item.did);

                for gp in &rustc_generics.params {
                    use rustc_middle::ty::GenericParamDefKind;
                    match gp.kind {
                        GenericParamDefKind::Lifetime => generic_counts.lifetimes += 1,
                        GenericParamDefKind::Type { .. } => generic_counts.types += 1,
                        GenericParamDefKind::Const { .. } => generic_counts.consts += 1,
                    }
                }
            }

            let for_tys = match impl_item.subject {
                ImplSubject::Inherent(ty) => {
                    let ty = vm.types.type_from_rustc(ty, &ctx);
                    SubList {
                        list: vec![Sub::Type(ty)],
                    }
                }
                ImplSubject::Trait(trait_ref) => vm.types.subs_from_rustc(trait_ref.substs, &ctx),
            };

            let key_ty = for_tys.list[0].assert_ty();

            let bounds = ImplBounds {
                for_tys,
                bounds,
                generic_counts,
            };

            let bounds_id = impls.add_bounds(bounds);

            match impl_item.subject {
                ImplSubject::Inherent(_) => {
                    if key_ty.kind().is_dummy() {
                        continue;
                    }

                    assert!(impl_item.assoc_tys.len() == 0);

                    for (member_key, val) in impl_item.assoc_values {
                        let mut full_key = key_ty
                            .impl_key()
                            .expect("inherent impls should always have a valid key")
                            .to_owned();
                        full_key.push_str(":");
                        full_key.push_str(&member_key);

                        impls.add_inherent(full_key, bounds_id, val);
                    }
                }
                ImplSubject::Trait(trait_ref) => {
                    let trait_did = trait_ref.def_id;

                    let trait_item = if let Some(trait_item) = ctx.item_by_did(trait_did) {
                        trait_item
                    } else {
                        let trait_path = path_from_rustc(&tcx.def_path(trait_did), vm);
                        let trait_crate_id = ctx.find_crate_id(trait_did.krate);
                        let trait_crate_items = vm.crate_provider(trait_crate_id);

                        trait_crate_items
                            .item_by_path(&trait_path)
                            .expect("couldn't find trait")
                    };

                    let assoc_value_count =
                        impl_item.assoc_values.len() + impl_item.assoc_tys.len();
                    let mut assoc_value_source = Vec::with_capacity(assoc_value_count);

                    for (key, val) in &impl_item.assoc_values {
                        assoc_value_source.push((ItemPath::for_value(key), val.clone()));
                    }
                    for (key, local_id) in &impl_item.assoc_tys {
                        let ty = tcx.type_of(*local_id).skip_binder();
                        let ty = vm.types.type_from_rustc(ty, &ctx);
                        assoc_value_source.push((ItemPath::for_type(key), AssocValue::Type(ty)));
                    }

                    let mut subject_key = key_ty.impl_key();

                    let assoc_values =
                        trait_item.trait_build_assoc_values_for_impl(&assoc_value_source);

                    impls.add_trait(trait_item, subject_key, bounds_id, assoc_values);
                }
            }
        }

        ctx.items.impls.set(impls).ok();

        if worker_config.crate_path.is_core() {
            let lang_items = tcx.lang_items();
            {
                let lang_trait = lang_items.sized_trait().unwrap();
                let lang_trait = ctx.item_by_did(lang_trait).unwrap();
                lang_trait.trait_set_builtin(BuiltinTrait::Sized);
            }
            {
                let lang_trait = lang_items.tuple_trait().unwrap();
                let lang_trait = ctx.item_by_did(lang_trait).unwrap();
                lang_trait.trait_set_builtin(BuiltinTrait::Tuple);
            }
            {
                let lang_trait = lang_items.discriminant_kind_trait().unwrap();
                let lang_trait = ctx.item_by_did(lang_trait).unwrap();
                lang_trait.trait_set_builtin(BuiltinTrait::DiscriminantKind);
            }
            {
                let lang_trait = lang_items.fn_once_trait().unwrap();
                let lang_trait = ctx.item_by_did(lang_trait).unwrap();
                lang_trait.trait_set_builtin(BuiltinTrait::FnOnce);
            }
            {
                let lang_trait = lang_items.fn_mut_trait().unwrap();
                let lang_trait = ctx.item_by_did(lang_trait).unwrap();
                lang_trait.trait_set_builtin(BuiltinTrait::FnMut);
            }
            {
                let lang_trait = lang_items.fn_trait().unwrap();
                let lang_trait = ctx.item_by_did(lang_trait).unwrap();
                lang_trait.trait_set_builtin(BuiltinTrait::Fn);
            }
            {
                let lang_trait = lang_items.pointee_trait().unwrap();
                let lang_trait = ctx.item_by_did(lang_trait).unwrap();
                lang_trait.trait_set_builtin(BuiltinTrait::Pointee);
            }
        }

        if vm.is_verbose {
            println!("item aggregation took {:?}", t.elapsed());
            println!("n = {}", ctx.items.items.len());
        }

        if worker_config.save_file {
            // force-generate all available IR
            for item in &ctx.items.items {
                // HACK: skip builtins
                let path = item.item.path.as_string();
                if path.starts_with("::_builtin::") {
                    continue;
                }

                let is_constant = !item.item.is_function();
                if let Some(ir) = build_ir(&ctx, item.did, is_constant) {
                    item.item.set_raw_ir(ir);
                }
            }

            let mut crate_header =
                PersistCrateHeader::from_rustc(ctx.tcx).expect("failed to build header");

            // TODO validate header by checking that none of the files were mutated after this executable started.
            // we could create two headers and compare them but, that would require knowing about all the source files
            // before rustc parses them

            // do not include files in header for internal crates
            if worker_config.crate_path.is_internal() {
                crate_header.files.clear();
            }

            let write_context = Rc::new(PersistWriteContext::new(this_crate));
            let mut writer = PersistWriter::new(write_context);
            persist_header_write(&mut writer);
            crate_header.persist_write(&mut writer);

            let items = ctx.items.items.iter().map(|item| item.item);
            LazyTable::<&Item>::write(&mut writer, items);

            /*let trait_impl_bytes = {
                let mut writer = writer.new_child_writer();

                impl_ids.len().persist_write(&mut writer);
                for (trait_item, index) in impl_ids {
                    trait_item.write_trait_impl(index, &mut writer);
                }

                writer.flip()
            };*/
            //panic!("todo impls");
            println!("todo impls");

            let types = writer.iter_types();
            LazyArray::<Type>::write(&mut writer, types);

            let cache_path = worker_config.crate_path.cache_path();
            std::fs::write(cache_path, writer.flip()).expect("save failed");
        }

        ctx
    }

    fn ctor_params(
        variant_data: &VariantData,
        adt_id: ItemId,
        variant_id: u32,
        vm: &'vm VM<'vm>,
        hir: &HirMap,
    ) -> Option<(ItemKind<'vm>, ItemPath<'vm>, LocalDefId)> {
        match variant_data {
            VariantData::Struct(..) => None,
            VariantData::Tuple(_, _, local_id) => {
                let path = path_from_rustc(&hir.def_path(*local_id), vm);
                let kind = ItemKind::new_function_ctor(adt_id, variant_id);

                Some((kind, path, *local_id))
            }
            VariantData::Unit(_, local_id) => {
                let path = path_from_rustc(&hir.def_path(*local_id), vm);
                let kind = ItemKind::new_const_ctor(adt_id, variant_id);

                Some((kind, path, *local_id))
            }
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
            self.items
                .map_defs
                .get(&local_did)
                .map(|item_id| self.items.items[item_id.index()].item)
        } else {
            None
        }
    }

    pub fn find_crate_id(&self, crate_num: rustc_span::def_id::CrateNum) -> CrateId {
        if crate_num == rustc_hir::def_id::LOCAL_CRATE {
            self.items.crate_id
        } else {
            let mut cache = self.extern_crate_id_cache.lock().unwrap();

            *cache.entry(crate_num).or_insert_with(|| {
                let crate_name = self.tcx.crate_name(crate_num);
                for entry in &self.extern_crates {
                    if entry.name == crate_name.as_str() {
                        return entry.id;
                    }
                }
                panic!(
                    "lookup for crate failed: {} {}",
                    crate_num,
                    crate_name.as_str()
                );
            })
        }
    }

    /// ONLY to be used for debugging local impls!
    pub fn find_inherent_impl(&self, ty: Type<'vm>, member_key: &str) -> Option<AssocValue<'vm>> {
        let ty_key = ty
            .impl_key()
            .expect("inherent impls must have a valid impl key!");
        let full_key = format!("{}:{}", ty_key, member_key);

        let impls = self.items.impls.get().expect("could not get impls");
        impls.find_inherent(&full_key, ty)
    }

    /// ONLY to be used for debugging local impls!
    pub fn get_item(&self, id: ItemId) -> Option<&'vm Item<'vm>> {
        if let Some(item) = self.items.items.get(id.index()) {
            Some(item.item)
        } else {
            None
        }
    }
}

struct RustCItems<'vm> {
    crate_id: CrateId,
    items: Vec<RustCItem<'vm>>,
    map_paths: AHashMap<ItemPath<'vm>, ItemId>,
    map_defs: AHashMap<LocalDefId, ItemId>,
    impls: OnceLock<ImplTableSimple<'vm>>,
}

struct RustCItem<'vm> {
    did: LocalDefId,
    item: &'vm Item<'vm>,
}

impl<'vm> RustCItems<'vm> {
    fn new(crate_id: CrateId) -> Self {
        RustCItems {
            crate_id,
            items: vec![],
            map_paths: AHashMap::new(),
            map_defs: AHashMap::new(),
            impls: Default::default(),
        }
    }

    fn index_item(
        &mut self,
        kind: ItemKind<'vm>,
        path: ItemPath<'vm>,
        did: LocalDefId,
        vm: &'vm VM<'vm>,
    ) -> ItemId {
        let item_id = ItemId::new(self.items.len() as u32);

        let item = vm.alloc_item(Item::new(vm, self.crate_id, item_id, path, kind));

        self.items.push(RustCItem { did, item });

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
