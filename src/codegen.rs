#![allow(warnings)]
use std::{collections::HashMap, env::set_var, panic};

use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicMetadataTypeEnum, BasicTypeEnum, FunctionType},
    values::{BasicValueEnum, FunctionValue, GlobalValue, PointerValue},
};

use crate::{
    expr::{self, Expr},
    stmt::Stmt,
};

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub variables: HashMap<String, (String, BasicTypeEnum<'ctx>, PointerValue<'ctx>)>,
    pub functions: HashMap<String, (String, FunctionValue<'ctx>)>,
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
        map.insert("printf".to_string(), ("int".to_string(), fn_print));
        Compiler {
            context,
            builder,
            module,
            functions: map,
            variables: HashMap::new(),
        }
    }

    pub fn get_type_func(
        &self,
        params: &Vec<(String, String)>,
    ) -> Vec<(String, String, BasicMetadataTypeEnum<'ctx>)> {
        let mut vec = vec![];
        for i in params {
            vec.push((i.1.clone(), i.0.clone(), self.get_basic_type(&i.0).into()));
        }
        vec
    }
    pub fn get_type(
        &mut self,
        params: &Vec<(String, String)>,
        type_: &str,
    ) -> (
        FunctionType<'ctx>,
        Vec<(String, String, BasicMetadataTypeEnum<'ctx>)>,
    ) {
        let t = self.get_type_func(params);
        let mut args = vec![];
        for i in &t {
            args.push(i.2.clone());
        }
        match type_ {
            "int" => {
                let i32_type = self.context.i32_type();
                let fn_type = i32_type.fn_type(&args, false);
                (fn_type, t)
            }
            "str" => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let fn_type = ptr_type.fn_type(&args, false);
                (fn_type, t)
            }
            "float" => {
                let ptr_type = self.context.f32_type();
                let fn_type = ptr_type.fn_type(&args, false);
                (fn_type, t)
            }
            "bool" => {
                let ptr_type = self.context.bool_type();
                let fn_type = ptr_type.fn_type(&args, false);
                (fn_type, t)
            }

            _ => panic!("error"),
        }
    }

    pub fn generate(&mut self, st: &[Stmt]) {
        for i in st {
            match i {
                Stmt::Func {
                    ret_type,
                    expr,
                    params,
                    name,
                } => {
                    let type_ = self.get_type(params, ret_type);
                    let function = self.module.add_function(name, type_.0, None);
                    let basic_block = self.context.append_basic_block(function, "entry");
                    self.builder.position_at_end(basic_block);
                    let locals = type_.1;
                    let mut type_loacls = HashMap::new();
                    for (idx, &(ref a, ref b, ref c)) in locals.iter().enumerate() {
                        let alloca = self.builder.build_alloca(self.get_basic_type(b), &a).unwrap();

                        if let Some(fn_param) = function.get_nth_param(idx as u32) {
                            self.builder.build_store(alloca, fn_param).unwrap();
                            type_loacls.insert(a.clone(), (b.clone(), fn_param.get_type(),alloca));
                        }
                    }
                    self.functions
                        .insert(name.clone(), (ret_type.to_string(), function));
                    for i in expr {
                        self.compile_statements(i, function, name.to_owned(), &type_loacls);
                    }
                }
                _ => panic!(""),
            }
        }
    }

    fn get_basic_type(&self, datatype: &str) -> BasicTypeEnum<'ctx> {
        match datatype {
            "int" => self.context.i32_type().into(),
            "float" => self.context.f32_type().into(),
            "bool" => self.context.bool_type().into(),
            "str" => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .into(),
            _ => panic!(""),
        }
    }

    pub fn compile_statements(
        &mut self,
        st: &Stmt,
        func: FunctionValue<'ctx>,
        name: String,
        type_loacls: &HashMap<String, (String, BasicTypeEnum<'ctx>,PointerValue<'ctx>)>,
    ) {
        match st {
            Stmt::EXPR_STMT { expr } => {
                expr.eval(self, type_loacls);
            }
            Stmt::Var {
                identifier,
                data_type,
                expr,
            } => {
                let data_t = self.get_basic_type(data_type);
                let alloca = self.builder.build_alloca(data_t, identifier).unwrap();
                self.variables.insert(
                    identifier.trim().to_string(),
                    (data_type.clone(), data_t, alloca),
                );

                let expr = expr.eval(self, type_loacls).unwrap();
                if expr.0.as_str() == data_type.as_str() {
                    self.builder.build_store(alloca, expr.1).unwrap();
                } else {
                    panic!("Expected types");
                }
            }

            Stmt::Func {
                ret_type,
                expr,
                params,
                name,
            } => {}
            Stmt::Return { exp } => {
                let eval = exp.eval(self, type_loacls).unwrap();
                let fn_ = self.functions.get(&name).unwrap();
                if eval.0.as_str() == "int"
                    && name.as_str() == "main"
                    && func.get_type().get_return_type().unwrap() == self.context.i32_type().into()
                {
                    self.builder.build_return(Some(&eval.1)).unwrap();
                } else if fn_.0 == eval.0 {
                    self.builder.build_return(Some(&eval.1)).unwrap();
                } else {
                    panic!("invalid return type ");
                }
            }

            // Stmt::Func_call { name } => {
            //     if let Some(a) = self.functions.get(name.trim()) {
            //         self.builder.build_call(*a, &[], "test_call").unwrap();
            //     } else {
            //         panic!("no fn with name {} found", name);
            //     }
            // }
            Stmt::print { format_argss, expr } => {
                let mut fmt = format_argss.clone();
                fmt = fmt.replace("\\n", "\n");
                fmt = fmt.replace("\\t", "\t");
                fmt = fmt.replace("%b", "%s");
                let format = self.builder.build_global_string_ptr(&fmt, "fmt").unwrap();
                let exp = expr.eval(self, type_loacls).unwrap();
                //println!("{:#?}",exp);
                let fns = self.functions.get("printf").unwrap().1;
                match exp.0.as_str() {
                    "str" => {
                        self.builder
                            .build_call(
                                fns,
                                &[format.as_pointer_value().into(), exp.1.into()],
                                "printf",
                            )
                            .unwrap();
                    }
                    "int" => {
                        let val = exp.1.get_type() == self.context.bool_type().into();
                        if !val {
                            self.builder
                                .build_call(
                                    fns,
                                    &[format.as_pointer_value().into(), exp.1.into()],
                                    "printf",
                                )
                                .unwrap();
                        } else {
                            let fnd = fns.clone();
                            self.bool_call(exp.1, format, &fnd);
                        }
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
                                fns,
                                &[format.as_pointer_value().into(), a.into()],
                                "printf",
                            )
                            .unwrap();
                    }
                    "bool" => {
                        let fnd = fns.clone();
                        self.bool_call(exp.1, format, &fnd);
                    }
                    _ => panic!("uknown_ type"),
                }
            }
        }
    }
    pub fn bool_call(
        &mut self,
        exp: BasicValueEnum<'ctx>,
        format: GlobalValue<'ctx>,
        fns: &FunctionValue<'ctx>,
    ) {
        let true_str = self
            .builder
            .build_global_string_ptr("true", "true_str")
            .unwrap();
        let false_str = self
            .builder
            .build_global_string_ptr("false", "false_str")
            .unwrap();
        let value = self
            .builder
            .build_select(exp.into_int_value(), true_str, false_str, "condi")
            .unwrap();

        self.builder
            .build_call(
                *fns,
                &[format.as_pointer_value().into(), value.into()],
                "printf",
            )
            .unwrap();
    }
}
