use std::{fs, path::PathBuf};

use crate::compiler::Compiler;

pub mod ast;
pub mod codegen;
pub mod compiler;
pub mod lexer;
pub mod parser;
pub mod type_checker;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: lucid <input.lucid>");
        std::process::exit(1);
    }

    let input_path = PathBuf::from(&args[1]);
    let source = fs::read_to_string(&input_path).expect("Failed to read input file");

    let mut compiler = Compiler::new();

    match compiler.compile(&source) {
        Ok(lua_code) => {
            // Output to stdout or write to file
            let output_path = input_path.with_extension("lua");
            fs::write(&output_path, lua_code).expect("Failed to write output file");
            println!("Compiled to: {}", output_path.display());
        }
        Err(e) => {
            eprintln!("Compilation error: {}", e);
            std::process::exit(1);
        }
    }
}
