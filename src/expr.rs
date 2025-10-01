use inkwell::{AddressSpace, values::BasicValueEnum};

use crate::codegen::{self, Compiler};

#[derive(Debug, Clone)]
#[allow(warnings)]
pub enum Literal {
    INT(i32),
    FLOAT(f32),
    String(String),
    Bool(bool),
    Nil,
}
impl Literal {
    pub fn compile_value<'ctx>(
        &self,
        compiler: &Compiler<'ctx>,
    ) -> Result<(String, BasicValueEnum<'ctx>), String> {
        match self {
            Literal::Bool(a)=> {
                if !*a {
                let tr = compiler.context.bool_type().const_int(0, false);
                Ok(("bool".to_owned(), tr.into()))
                }else {
                 let tr = compiler.context.bool_type().const_int(1, false);
                Ok(("bool".to_owned(), tr.into()))
   
                }
            }
            Literal::Nil => {
                let tr = compiler.context.ptr_type(AddressSpace::default());
                let ptr = tr.const_null();
                Ok(("null".to_owned(), ptr.into()))
            }
            Literal::FLOAT(a) => Ok((
                "float".to_owned(),
                compiler.context.f32_type().const_float(*a as f64).into(),
            )),
            Literal::INT(a) => Ok((
                "int".to_owned(),
                compiler
                    .context
                    .i32_type()
                    .const_int(*a as u64, true)
                    .into(),
            )),
            Literal::String(a) => {
                let str_ptr = compiler
                    .builder
                    .build_global_string_ptr(a.as_str(), "def_str")
                    .unwrap();
                Ok(("str".to_owned(), str_ptr.as_pointer_value().into()))
            }
        }
    }
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum Expr {
    Literal {
        value: Literal,
    },
    Group {
        value: Box<Expr>,
    },
    Unary {
        op: String,
        expr: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: String,
        right: Box<Expr>,
    },
}
macro_rules! Operator {
   (int, $c:expr, $op:tt, $lhs:expr, $rhs:expr, $name:expr) => {{
        match $op {
            "+" => $c.builder.build_int_add($lhs.into_int_value(), $rhs.into_int_value(), $name),
            "-" => $c.builder.build_int_sub($lhs.into_int_value(), $rhs.into_int_value(), $name),
            "*" => $c.builder.build_int_mul($lhs.into_int_value(), $rhs.into_int_value(), $name),
            "/" => $c.builder.build_int_signed_div($lhs.into_int_value(), $rhs.into_int_value(), $name),
            _ => panic!("Unsupported integer operator: "),
        }
        .unwrap()
        .into()
    }};
   (float, $c:expr, $op:tt, $lhs:expr, $rhs:expr, $name:expr) =>{{
    match $op {
            "+" => $c.builder.build_float_add($lhs.into_float_value(), $rhs.into_float_value(), $name),
            "-" => $c.builder.build_float_sub($lhs.into_float_value(), $rhs.into_float_value(), $name),
            "*" => $c.builder.build_float_mul($lhs.into_float_value(), $rhs.into_float_value(), $name),
            "/" => $c.builder.build_float_div($lhs.into_float_value(), $rhs.into_float_value(), $name),
            _ => panic!("Unsupported integer operator: "),
        }
        .unwrap()
        .into()

   }}; 
}
impl Expr {
    pub fn eval<'ctx>(
        &self,
        compiler: &codegen::Compiler<'ctx>,
    ) -> Result<(String, BasicValueEnum<'ctx>), String> {
        match self {
            Expr::Literal { value } => value.compile_value(compiler),
            Expr::Group { value } => value.eval(compiler),
            Expr::Binary { left, op, right } => {
                let left = left.eval(compiler)?;
                let right = right.eval(compiler)?;

                match (left.0.as_str(), op.as_str(), right.0.as_str()) {
                    ("float", a, "float") => Ok((
                        right.0.clone(),
                        Operator!(float,compiler,a,left.1,right.1,"temp_op")
                    )),
                                      ("int", a, "int") => Ok((
                        right.0.clone(),
                        Operator!(int,compiler,a,left.1,right.1,"temp_op")
                    )),
                    

                    (a, b, c) => Err(format!("cant evaluate {a} {b} {c}")),
                }
            }
            Expr::Unary { op, expr } => {
                let value = expr.eval(compiler)?;
                match (op.as_str(), value.0.as_str()) {
                    ("-", "int") => {
                        let const_value = compiler.context.i32_type().const_int(1 as u64, true);
                        Ok((
                            "int".to_string(),
                            compiler
                                .builder
                                .build_int_mul(
                                    value.1.into_int_value(),
                                    const_value.const_neg(),
                                    "not",
                                )
                                .unwrap()
                                .into(),
                        ))
                    }
                    (a, b) => Err(format!("cant operate on {a}{b}")),
                }
            }
        }
    }
}
