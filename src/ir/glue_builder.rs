use crate::{types::{Type, TypeKind}, ir::{Pattern, PatternKind, BindingMode, IRFunctionBuilder, ExprKind, Expr}};

use super::IRFunction;

/// used to generate glue code for Fn* traits on regular functions
pub fn glue_for_fn_trait<'vm>(
    func_ty: Type<'vm>,
    self_ty: Type<'vm>,
    args_ty: Type<'vm>,
    res_ty: Type<'vm>,
) -> IRFunction<'vm> {
    assert!(func_ty.is_concrete());
    assert!(args_ty.is_concrete());

    let mut builder = IRFunctionBuilder::default();

    let params = vec![self_ty, args_ty]
        .iter()
        .enumerate()
        .map(|(i, ty)| builder.add_pattern(Pattern {
            kind: PatternKind::LocalBinding {
                local_id: i as u32,
                mode: BindingMode::Value,
                sub_pattern: None,
            },
            ty: *ty,
        }))
        .collect();

    let TypeKind::Tuple(inner_arg_tys) = args_ty.kind() else {
        panic!("fn trait args must be a tuple");
    };

    // build the actual ir
    let func_expr = builder.add_expr(Expr {
        kind: ExprKind::LiteralVoid,
        ty: func_ty,
    });

    let tuple_expr = builder.add_expr(Expr {
        kind: ExprKind::VarRef(1),
        ty: args_ty,
    });

    let mut call_args = Vec::new();
    for (i, arg_ty) in inner_arg_tys.iter().enumerate() {
        call_args.push(builder.add_expr(Expr {
            kind: ExprKind::Field {
                lhs: tuple_expr,
                variant: 0,
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

    builder.finish(root_expr, false, params)
}
