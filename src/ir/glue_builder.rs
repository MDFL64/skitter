use crate::{
    closure::FnTrait,
    ir::{BindingMode, Expr, ExprKind, IRFunctionBuilder, Pattern, PatternKind},
    types::{Mutability, Type, TypeKind},
    variants::VariantIndex,
};

use super::{IRFunction, IRKind};

/// used to generate glue code for Fn* traits on regular functions
pub fn glue_for_fn_trait<'vm>(
    kind: FnTrait,
    func_ty: Type<'vm>,
    args_ty: Type<'vm>,
    res_ty: Type<'vm>,
) -> IRFunction<'vm> {
    assert!(func_ty.is_concrete());
    assert!(args_ty.is_concrete());

    let self_ty = match kind {
        FnTrait::Fn => func_ty.ref_to(Mutability::Const),
        FnTrait::FnMut => func_ty.ref_to(Mutability::Mut),
        FnTrait::FnOnce => func_ty,
    };

    let mut builder = IRFunctionBuilder::default();

    let params = vec![self_ty, args_ty]
        .iter()
        .enumerate()
        .map(|(i, ty)| {
            builder.add_pattern(Pattern {
                kind: PatternKind::LocalBinding {
                    local_id: i as u32,
                    mode: BindingMode::Value,
                    sub_pattern: None,
                },
                ty: *ty,
            })
        })
        .collect();

    let TypeKind::Tuple(inner_arg_tys) = args_ty.kind() else {
        panic!("fn trait args must be a tuple");
    };

    // build the actual ir
    let mut func_expr = builder.add_expr(Expr {
        kind: ExprKind::VarRef(0),
        ty: self_ty,
    });

    match kind {
        FnTrait::Fn | FnTrait::FnMut => {
            func_expr = builder.add_expr(Expr {
                kind: ExprKind::DeRef(func_expr),
                ty: func_ty,
            });
        }
        FnTrait::FnOnce => (),
    }

    let tuple_expr = builder.add_expr(Expr {
        kind: ExprKind::VarRef(1),
        ty: args_ty,
    });

    let mut call_args = Vec::new();
    for (i, arg_ty) in inner_arg_tys.iter().enumerate() {
        call_args.push(builder.add_expr(Expr {
            kind: ExprKind::Field {
                lhs: tuple_expr,
                variant: VariantIndex::new(0),
                field: i as u32,
            },
            ty: *arg_ty,
        }));
    }

    let root_expr = builder.add_expr(Expr {
        kind: ExprKind::Call {
            func: func_expr,
            args: call_args,
        },
        ty: res_ty,
    });

    builder.finish(root_expr, IRKind::Function, params, vec![])
}

pub fn glue_for_ctor<'vm>(
    adt_ty: Type<'vm>,
    variant: VariantIndex,
    ir_kind: IRKind,
) -> IRFunction<'vm> {
    let TypeKind::Adt(item_with_subs) = adt_ty.kind() else {
        panic!("attempt to get ctor for non-adt");
    };

    let adt_info = item_with_subs.item.adt_info();

    let mut builder = IRFunctionBuilder::default();

    let params: Vec<_> = adt_info
        .variant_fields
        .get(variant)
        .iter()
        .enumerate()
        .map(|(i, ty)| {
            let ty = ty.sub(&item_with_subs.subs);

            builder.add_pattern(Pattern {
                kind: PatternKind::LocalBinding {
                    local_id: i as u32,
                    mode: BindingMode::Value,
                    sub_pattern: None,
                },
                ty,
            })
        })
        .collect();

    let fields = params
        .iter()
        .enumerate()
        .map(|(i, pat_id)| {
            let expr_id = builder.add_expr(Expr {
                kind: ExprKind::VarRef(i as u32),
                ty: builder.pattern(*pat_id).ty,
            });
            (i as u32, expr_id)
        })
        .collect();

    let struct_expr = builder.add_expr(Expr {
        kind: ExprKind::Adt {
            variant,
            fields,
            rest: None,
        },
        ty: adt_ty,
    });

    builder.finish(struct_expr, ir_kind, params, vec![])
}
