use crate::ir::{BinaryOp, Stmt, MatchGuard};

use super::{Block, ExprId, ExprKind, IRFunction, PatternId, PatternKind, UnaryOp};

impl<'vm> IRFunction<'vm> {
    pub fn print(&self) {
        print!("f( ");
        for (i, p) in self.params.iter().enumerate() {
            if i > 0 {
                print!(", ");
            }
            self.print_pattern(*p)
        }
        print!(" ) ");

        self.print_expr(self.root_expr, 0);

        println!();
    }

    fn print_block(&self, block: &Block, indent: u32) {
        let sub_indent = indent + 1;
        println!("{{");
        for stmt in block.stmts.iter() {
            for _ in 0..sub_indent {
                print!("   ");
            }
            match stmt {
                Stmt::Expr(e) => {
                    self.print_expr(*e, sub_indent);
                    println!(";");
                }
                Stmt::Let {
                    pattern,
                    init,
                    else_block,
                } => {
                    print!("let ");
                    self.print_pattern(*pattern);
                    if let Some(init) = init {
                        print!(" = ");
                        self.print_expr(*init, sub_indent);
                    }
                    assert!(else_block.is_none());
                    println!(";");
                }
            }
        }

        if let Some(res) = block.result {
            for _ in 0..sub_indent {
                print!("   ");
            }
            self.print_expr(res, sub_indent);
            println!();
        }

        for _ in 0..indent {
            print!("   ");
        }
        print!("}}");
    }

    fn print_expr(&self, id: ExprId, indent: u32) {
        let expr = self.expr(id);
        match expr.kind {
            ExprKind::LiteralValue(n) => {
                print!("{}{}", n, expr.ty);
            }
            ExprKind::LiteralVoid => {
                print!("{}", expr.ty);
            }
            ExprKind::VarRef(local_id) => {
                print!("var{}", local_id);
            }
            ExprKind::NamedConst(ref item) => {
                print!("{}", item);
            }
            ExprKind::DeRef(child) => {
                print!("*");
                self.print_expr(child, indent);
            }
            ExprKind::Ref(child, _) => {
                print!("&");
                self.print_expr(child, indent);
            }
            ExprKind::Unary(op, child) => {
                let op = match op {
                    UnaryOp::Neg => '-',
                    UnaryOp::Not => '!',
                };
                print!("{}", op);
                self.print_expr(child, indent);
            }
            ExprKind::Binary(op, lhs, rhs) => {
                let op = match op {
                    BinaryOp::Sub => "-",

                    BinaryOp::Eq => "==",
                    BinaryOp::Lt => "<",
                    BinaryOp::LtEq => "<=",
                    BinaryOp::GtEq => ">=",
                    _ => panic!("op {:?}", op),
                };
                print!("( ");
                self.print_expr(lhs, indent);
                print!(" {} ", op);
                self.print_expr(rhs, indent);
                print!(" )");
            }
            ExprKind::Cast(child) => {
                print!("( ");
                self.print_expr(child, indent);
                print!(" as {} )", expr.ty);
            }
            ExprKind::Call { func, ref args } => {
                self.print_expr(func, indent);
                print!("( ");

                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        print!(", ");
                    }
                    self.print_expr(*a, indent);
                }

                print!(" )");
            }
            ExprKind::Tuple(ref children) => {
                print!("( ");

                for (i, a) in children.iter().enumerate() {
                    if i > 0 {
                        print!(", ");
                    }
                    self.print_expr(*a, indent);
                }

                print!(" )");
            }
            ExprKind::Block(ref block) => {
                self.print_block(block, indent);
            }
            ExprKind::If {
                cond,
                then,
                else_opt,
            } => {
                print!("if ");
                self.print_expr(cond, indent);
                print!(" ");
                self.print_expr(then, indent);
                if let Some(else_expr) = else_opt {
                    print!(" else ");
                    self.print_expr(else_expr, indent);
                }
            }
            ExprKind::Match { arg, ref arms } => {
                print!("match ");
                self.print_expr(arg, indent);
                println!("{{");

                let sub_indent = indent + 1;

                for a in arms {
                    for _ in 0..sub_indent {
                        print!("   ");
                    }
                    match a.guard {
                        MatchGuard::None => (),
                        _ => panic!("print match guard")
                    }
                    self.print_pattern(a.pattern);
                    print!(" => ");
                    self.print_expr(a.body, sub_indent);
                    println!();
                }

                for _ in 0..indent {
                    print!("   ");
                }
                print!("}}");
            }
            _ => print!("^{:?}", expr.kind),
        }
    }

    fn print_pattern(&self, id: PatternId) {
        let pat = self.pattern(id);
        match pat.kind {
            PatternKind::LocalBinding {
                local_id,
                mode,
                sub_pattern,
            } => {
                assert!(sub_pattern.is_none());
                print!("var{}", local_id);
            }
            PatternKind::Hole => print!("_"),
            _ => print!("^{:?}", pat.kind),
        }
    }
}
