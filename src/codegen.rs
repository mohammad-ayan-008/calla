use std::{collections::HashMap, env::set_var};

use inkwell::{
    AddressSpace, builder::Builder, context::Context, module::Module, types::FunctionType,
    values::FunctionValue,
};

use crate::{expr::{self, Expr}, stmt::Stmt};

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    functions: HashMap<String, FunctionValue<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let fn_type = context
            .i8_type()
            .fn_type(&[context.ptr_type(AddressSpace::default()).into()], true);
        let fn_print = module.add_function("printf", fn_type, None);
        let mut map = HashMap::new();
        map.insert("printf".to_string(), fn_print);
        Compiler {
            context,
            builder,
            module,
            functions: map,
        }
    }

    pub fn generate(&mut self, st: &[Stmt]) {
        for i in st {
            match i {
                Stmt::Func {
                    ret_type,
                    expr,
                    name,
                } => {
                    let i32_type = self.context.i32_type();
                    let fn_type = i32_type.fn_type(&[], false);
                    let function = self.module.add_function(name, fn_type, None);
                    let basic_block = self.context.append_basic_block(function, "entry");
                    self.builder.position_at_end(basic_block);
                    for i in expr {
                        self.compile_statements(i, function);
                    }
                }
                _ => panic!(""),
            }
        }
    }

    pub fn compile_statements(&mut self, st: &Stmt, func: FunctionValue<'ctx>) {
        match st {
            Stmt::Func {
                ret_type,
                expr,
                name,
            } => {}
            Stmt::Return { exp } => {
                let eval = exp.eval(self).unwrap();
                if eval.0.as_str() == "int"
                    && func.get_type().get_return_type().unwrap() == self.context.i32_type().into()
                {
                    self.builder.build_return(Some(&eval.1)).unwrap();
                } else {
                    panic!("invalid return type");
                }
            }
            Stmt::print { format_argss, expr } => {
                let mut fmt = format_argss.clone();
                fmt = fmt.replace("\\n", "\n");
                fmt = fmt.replace("\\t", "\t");
                let format = self.builder.build_global_string_ptr(&fmt, "fmt").unwrap();
                let exp = expr.eval(self).unwrap();

                let fns = self.functions.get("printf").unwrap();
                match exp.0.as_str() {
                    "int" => {
                        self.builder
                            .build_call(
                                *fns,
                                &[format.as_pointer_value().into(), exp.1.into()],
                                "printf",
                            )
                            .unwrap();
                    }
                    "float" => {
                        let a = self
                            .builder
                            .build_float_ext(
                                exp.1.into_float_value(),
                                self.context.f64_type(),
                                "promoted",
                            )
                            .unwrap();
                        self.builder
                            .build_call(
                                *fns,
                                &[format.as_pointer_value().into(), a.into()],
                                "printf",
                            )
                            .unwrap();
                    }
                    "bool" => {
                        self.builder
                            .build_call(
                                *fns,
                                &[format.as_pointer_value().into(), exp.1.into()],
                                "printf",
                            )
                            .unwrap();
                    }
                    _ => panic!("uknown_ type"),
                }
            }
        }
    }
}
