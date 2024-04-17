// This is to build our program
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

    println!("[Status] Building and linking {}.exe successful.",name);

}
