// This is to build our program

use crate::*;

pub fn tokens_to_asm(tokens: &Vec<Token>) -> String {
    let mut out = String::from(
        "section .text\n\tglobal _start\n\t
    extern ExitProcess\n_start:\n\t
",
    );
    let mut i = 0;
    while i < tokens.len() {
        let token = &tokens[i];
        if token.token_type == TokenType::EXIT
            && i + 1 < tokens.len()
            && tokens[i + 1].token_type == TokenType::NUM
        {
            let num = tokens[i + 1]
                .value
                .as_ref()
                .unwrap()
                .as_str()
                .parse::<u32>()
                .unwrap();
            if i + 2 < tokens.len() && tokens[i + 2].token_type == TokenType::SEMICOLON {
                out = format!(
                    "{}\n\tmov rcx, {}\n\tsub rsp, 32\n\t call ExitProcess",
                    out, num
                );
            }
        }
        i += 1;
    }
    out
}

pub fn build(name: &str) {
    use std::process::Command;
    // nasm -f win64 your_program.asm -o your_program.obj
    let nasm_args = format!("-f win64 {}.asm -o {}.obj", name, name);
    let build_obj_command = Command::new("nasm")
        .args(nasm_args.split_whitespace())
        .output();

    match build_obj_command {
        Err(e) => {
            println!("Unable to build: {:?}", e);
            return;
        }
        _ => (),
    }

    // ld your_program.obj -o your_program.exe -e _start -lkernel32
    let ld_args = format!(
        "{}.obj -o {}.exe -e _start -lkernel32 -L c:/path/to/kernel32",
        name, name
    );
    let linker_command = Command::new("ld").args(ld_args.split_whitespace()).output();

    match linker_command {
        Err(e) => {
            println!("Unable to link: {:?}", e);
            return;
        }
        _ => (),
    }

    println!("Building and linking successful!");
}
