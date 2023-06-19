use super::{ExprId, ExprKind, IRFunction};

#[derive(PartialEq)]
pub enum ConstStatus {
    Explicit,
    CanPromote,
    Not,
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

            ExprKind::VarRef(..) => false,

            _ => panic!("is_const_alloc {:?}", expr.kind),
        }
    }
}
