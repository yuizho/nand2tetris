use std::env;
use std::fs::File;
use std::io::{BufWriter, Write};
mod parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = get_filename(&args);

    let mut parser = parser::Parser::create(filename);
    let compiled_filename = filename.replace(".asm", ".hack");
    let mut f =
        BufWriter::new(File::create(compiled_filename).expect("failed to create hack file."));
    while parser.has_more_commands() {
        // parse nimonic to commant_type obj
        let command_type = parser.command_type();
        // convert command_type obj to binary
        let code = parser::convert_code(command_type);
        if let Some(code) = code {
            let binary_instruction = format!("{:016b}", code) + "\n";
            f.write(binary_instruction.as_bytes())
                .expect("failed to write hack file");
        }
        parser.advance();
    }
    f.flush().expect("failed to write hack file");
}

fn get_filename(args: &Vec<String>) -> &String {
    if args.len() < 2 {
        panic!("this command needs at least one argument")
    }
    if !args[1].ends_with(".asm") {
        panic!("the file is not expected assembly file.")
    }
    &args[1]
}
