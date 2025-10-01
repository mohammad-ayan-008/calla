use std::{collections::HashMap, env::set_var};

use inkwell::{builder::Builder, context::Context, module::Module, values::FunctionValue, AddressSpace};

use crate::{expr::Expr, stmt::Stmt};

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    functions: HashMap<String,FunctionValue<'ctx>> 
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let fn_type = context.i8_type().fn_type(&[context.ptr_type(AddressSpace::default()).into()],true);
        let fn_print = module.add_function("printf", fn_type, None);
        let mut map = HashMap::new();
        map.insert("printf".to_string(), fn_print);
        Compiler {
            context,
            builder,
            module,
            functions:map
        }
    }

    pub fn generate(&mut self, st: &[Stmt]) {
        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        for i in st{
            self.compile_statements(i);
        }
        // returning 0
        let _ = self.builder.build_return(Some(&self.context.i8_type().const_zero()));
    }

    pub fn compile_statements(&mut self,st:&Stmt){
        match st {
            Stmt::print { format_argss, expr }=>{
                let mut fmt = format_argss.clone();
                fmt = fmt.replace("\\n", "\n"); 
                fmt = fmt.replace("\\t", "\t"); 
                let format = self.builder.build_global_string_ptr(&fmt,"fmt").unwrap();
                let exp = expr.eval(self).unwrap();

                let fns =self.functions.get("printf").unwrap();
                match exp.0.as_str() {
                    "int"=>{
                        self.builder.build_call(*fns, &[format.as_pointer_value().into(),exp.1.into()], "printf").unwrap();
                    },
                    "float"=>{
                        let a =self.builder.build_float_ext(exp.1.into_float_value(), self.context.f64_type(), "promoted").unwrap();
                        self.builder.build_call(*fns, &[format.as_pointer_value().into(),a.into()], "printf").unwrap();
                    },
                    "bool"=>{
                        self.builder.build_call(*fns, &[format.as_pointer_value().into(),exp.1.into()], "printf").unwrap();

                    },
                    _=>panic!("uknown_ type")
                }



            }
        }
    }
}
