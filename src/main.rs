use std::env;
use std::fs;
use std::fs::File;
use std::io::prelude::*;

mod builder;
mod generator;
mod parser;
mod tokens;

use builder::*;
use generator::*;
use parser::*;
use tokens::*;

fn main() {
    println!("Check todos!. main.rs");
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Incorrect Usage. Correct Usage: \n\t toylang <input_file>.tl");
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
