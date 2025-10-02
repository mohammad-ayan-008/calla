use std::{collections::HashMap, env::set_var, panic};

use inkwell::{
    builder::Builder, context::Context, module::Module, types::{BasicTypeEnum, FunctionType}, values::{BasicValueEnum, FunctionValue, GlobalValue, PointerValue}, AddressSpace
};

use crate::{expr::{self, Expr}, stmt::Stmt};

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub variables: HashMap<String, (String, BasicTypeEnum<'ctx>, PointerValue<'ctx>)>,
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
            variables:HashMap::new()
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

     fn get_basic_type(&self, datatype: &str) -> BasicTypeEnum<'ctx> {
        match datatype {
            "int" => self.context.i32_type().into(),
            "float" => self.context.f32_type().into(),
            "bool" => self.context.bool_type().into(),
            "str" => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .into(),
            _ => panic!("")
              
            }
        }
    
    pub fn compile_statements(&mut self, st: &Stmt, func: FunctionValue<'ctx>) {
        match st {
            Stmt::Var { identifier, data_type, expr }=>{
                let data_t = self.get_basic_type(data_type);
                let alloca = self.builder.build_alloca(data_t, identifier).unwrap();
                self.variables.insert(identifier.trim().to_string(), (data_type.clone(),data_t,alloca));

                let expr = expr.eval(self).unwrap();
                if expr.0.as_str() == data_type.as_str(){
                    self.builder.build_store(alloca, expr.1).unwrap();
                }else {
                    panic!("Expected types");
                }

            },
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
                fmt = fmt.replace("%b", "%s");
                let format = self.builder.build_global_string_ptr(&fmt, "fmt").unwrap();
                let exp = expr.eval(self).unwrap();
                println!("{:#?}",exp);
                let fns = self.functions.get("printf").unwrap();
                match exp.0.as_str() {
                    "str" =>{
                        self.builder
                        .build_call(*fns,&[format.as_pointer_value().into() ,exp.1.into()],
                                "printf").unwrap();
                    }
                    "int" => {
                        let val = exp.1.get_type() == self.context.bool_type().into();
                        if !val{
                        self.builder
                            .build_call(
                                *fns,
                                &[format.as_pointer_value().into(), exp.1.into()],
                                "printf",
                            )
                            .unwrap();
                        }else {
                          let fnd= fns.clone();
                          self.bool_call(exp.1,format,&fnd);
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
                                *fns,
                                &[format.as_pointer_value().into(), a.into()],
                                "printf",
                            )
                            .unwrap();
                    }
                    "bool" => {
                          let fnd= fns.clone();
                          self.bool_call(exp.1,format,&fnd);
                    }
                    _ => panic!("uknown_ type"),
                }
            }
        }
    }
    pub fn bool_call(&mut self,exp:BasicValueEnum<'ctx>,format:GlobalValue<'ctx>,fns:&FunctionValue<'ctx>){
                        let true_str = self.builder.build_global_string_ptr("true", "true_str").unwrap();
                        let false_str = self.builder.build_global_string_ptr("false", "false_str").unwrap(); 
                        let value = self.builder.build_select(exp.into_int_value(), true_str, false_str, "condi").unwrap();

                        self.builder
                            .build_call(
                                *fns,
                                &[format.as_pointer_value().into(), value.into()],
                                
                                "printf",
                            )
                            .unwrap();
    }
        
}

