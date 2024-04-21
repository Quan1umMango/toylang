use std::env;
use std::fs;
use std::fs::File;
use std::io::prelude::*;

mod builder;
mod generator;
mod parser;
mod tokens;
mod erroring;
mod generate_bytecode;

use builder::*;
use generator::*;
use parser::*;
use tokens::*;
use erroring::*;
use generate_bytecode::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 3 {
        println!("Incorrect Usage. Correct Usage: \n\t toylang <input_file>.tl <runtype>");
        return;
    }

    let file_path = &args[1];
    let file_name = file_path.as_str().get(0..file_path.len() - 3).unwrap();
    let extension = file_path.as_str().get(file_path.len() - 3..);

    if extension != Some(".tl") {
        println!("Incorrect file type provided. File must be a .tl file.");
        return;
    }
    let contents = {
        let c = fs::read_to_string(file_path);
        match c {
            Ok(v) => v,
            Err(e) => {
                eprintln!("Error in getting input file: {}", e.to_string());
                return;
            }
        }
    };
    let mut tokenizer = Tokenizer::new(contents);
    let tokens = tokenizer.tokenize();

    let mut parser = Parser::new(tokens);
    let prog = parser.parse_program().unwrap();

    let runtype = &args[2];
    match runtype.as_str() {
        "bytecode" | "b" => {
            let mut generator = GeneratorVM::new(prog);
            generator.generate();
            let asm = generator.output();
            let mut file = File::create(format!("{}.basm",file_name)).ok().unwrap();
            match file.write_all(asm.as_bytes()) {
                Err(e) => {
                    println!("Unable to write to file {:?}",e);
                    std::process::exit(1);
                }
                Ok(_) => () 
            }

            println!("[Status] Finished Generating {}.basm file.",file_name);
        }
        "compiler" | "c" => {
            
            println!("[WARNING] Development to native compilation will be slowed down in favour of compiling to bytecode.\n\t  Some features may not be available.\n\t  Use the `i` flag for future uses.");
            let mut generator = Generator::new(prog);
            let asm = generator.generate_program();
            let mut file = File::create(format!("{}.asm", file_name)).ok().unwrap();
            match file.write_all(asm.as_bytes()) {
                Err(e) => {
                    println!("Unable to Write to file: {:?}", e)
                }
                _ => (),
            }
            build(file_name);
        }
        _ => {eprintln!("Invalid RunType. Expected either `bytecode` or `compiler`."); return}
    }


}
