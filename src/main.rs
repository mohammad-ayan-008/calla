use std::{cell::RefCell, env::set_var, fmt::format, fs::read_to_string, path::Path, rc::Rc};

use inkwell::{
    context::Context,
    targets::{self, InitializationConfig, Target, TargetMachine},
};

use crate::{codegen::Compiler, parser_calla::parse_expr_stmts};

mod codegen;
mod expr;
mod parser_calla;
mod stmt;

pub fn compile_statement() {}

fn main() {
    let data = read_to_string("test.calla").unwrap();
    let ast = parse_expr_stmts(&data).unwrap();
    let context = Context::create();
    let mut codegen = Compiler::new(&context, "main_s");
    codegen.generate(&ast);

    inkwell::targets::Target::initialize_all(&InitializationConfig::default());
    let target = Target::from_triple(&TargetMachine::get_default_triple()).unwrap();

    let target_machine = target
        .create_target_machine(
            &TargetMachine::get_default_triple(),
            "generic",
            "",
            inkwell::OptimizationLevel::Default,
            targets::RelocMode::PIC,
            targets::CodeModel::Default,
        )
        .unwrap();

    println!("{}", codegen.module.print_to_string().to_string());
    target_machine
        .write_to_file(
            &codegen.module,
            targets::FileType::Object,
            Path::new("output.o"),
        )
        .unwrap();
}
