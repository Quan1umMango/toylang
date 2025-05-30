use std::env;
use std::fs;
use std::fs::File;
use std::io::prelude::*;

mod builder;
mod parser;
mod tokens;
mod erroring;
//mod generate_bytecode;
mod generate_bytecode2;
//mod intermediate_lang; 

use builder::*;
use parser::*;
use tokens::*;
use erroring::*;
//use generate_bytecode::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        println!("Incorrect Usage. Correct Usage: \n\t toylang <input_file>.tl >");
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
/*
    let mut generator = GeneratorVM::new(prog,parser.functions,parser.vars);
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
    */
    let mut gen = generate_bytecode2::GeneratorVM::new(prog,parser.functions,parser.vars);
    gen.generate();
    let asm = gen.output();
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
