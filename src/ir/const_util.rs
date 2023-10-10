use crate::types::Mutability;

use super::{ExprId, ExprKind, IRFunction};

#[derive(PartialEq)]
pub enum ConstStatus {
    Explicit,
    CanPromote,
    Not,
}

impl ConstStatus {
    pub fn is_const(&self) -> bool {
        match self {
            ConstStatus::Explicit | ConstStatus::CanPromote => true,
            ConstStatus::Not => false,
        }
    }
}

impl<'vm> IRFunction<'vm> {
    pub fn is_const_alloc(&self, id: ExprId) -> bool {
        let expr = self.expr(id);
        match expr.kind {
            ExprKind::NamedConst(..) => true,

            ExprKind::DeRef(child)
            | ExprKind::Ref(child, _)
            | ExprKind::Field { lhs: child, .. }
            | ExprKind::Index { lhs: child, .. } => self.is_const_alloc(child),

            ExprKind::VarRef(..) | ExprKind::UpVar(_) => false,

            ExprKind::Call { .. } => false,

            _ => panic!("is_const_alloc {:?} / {}", expr.kind, expr.ty),
        }
    }

    pub fn const_status(&self, id: ExprId) -> ConstStatus {
        let expr = self.expr(id);
        match expr.kind {
            ExprKind::VarRef(_) => ConstStatus::Not,
            ExprKind::UpVar(_) => ConstStatus::Not,

            ExprKind::LiteralValue(_) => ConstStatus::CanPromote,
            ExprKind::LiteralVoid => ConstStatus::Not, // ???

            ExprKind::LiteralBytes(_) => ConstStatus::Explicit,

            ExprKind::Ref(child, mutability) => {
                if mutability == Mutability::Const && self.const_status(child).is_const() {
                    ConstStatus::CanPromote
                } else {
                    ConstStatus::Not
                }
            }

            ExprKind::NamedConst(_) => ConstStatus::Explicit,
            ExprKind::Static(_) => ConstStatus::Not,

            ExprKind::Adt { ref fields, .. } => {
                let all_const = fields
                    .iter()
                    .all(|(_, id)| self.const_status(*id).is_const());

                if all_const {
                    ConstStatus::CanPromote
                } else {
                    ConstStatus::Not
                }
            }
            ExprKind::Tuple(ref fields) | ExprKind::Array(ref fields) => {
                let all_const = fields.iter().all(|id| self.const_status(*id).is_const());

                if all_const {
                    ConstStatus::CanPromote
                } else {
                    ConstStatus::Not
                }
            }

            ExprKind::Field { lhs: child, .. } | ExprKind::Cast(child) | ExprKind::DeRef(child) => {
                if self.const_status(child).is_const() {
                    ConstStatus::CanPromote
                } else {
                    ConstStatus::Not
                }
            }

            ExprKind::Binary(_, lhs, rhs) => {
                let args_const =
                    self.const_status(lhs).is_const() && self.const_status(rhs).is_const();

                if args_const {
                    panic!("todo arg types must be primitive")
                } else {
                    ConstStatus::Not
                }
            }

            ExprKind::Call { .. } => ConstStatus::Not,

            ExprKind::Block(ref block) => {
                if let Some(res) = block.result {
                    if self.const_status(res).is_const() {
                        if block.stmts.len() > 0 {
                            panic!("const-promoted block may have side-effects!");
                        }

                        ConstStatus::CanPromote
                    } else {
                        ConstStatus::Not
                    }
                } else {
                    ConstStatus::Not
                }
            }

            _ => panic!("const_status {:?}", expr.kind),
        }
    }
}
