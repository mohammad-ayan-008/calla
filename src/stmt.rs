use crate::expr::{Expr, Literal};

#[derive(Debug)]
pub enum Stmt {
    Func {
        ret_type: String,
        expr: Vec<Stmt>,
        name: String,
    },
    Var{
        identifier:String,
        data_type:String,
        expr:Expr
    },
    Return {
        exp: Expr,
    },
    print {
        format_argss: String,
        expr: Expr,
    },
}
