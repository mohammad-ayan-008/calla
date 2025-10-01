use crate::expr::Expr;

#[derive(Debug)]
pub enum Stmt {
    print{
        format_argss:String,
        expr:Expr
    }
}
