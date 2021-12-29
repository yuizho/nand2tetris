use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Seek, Write};

mod translator;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = get_filename(&args);

    let f = File::open(filename).expect("file not found");
    let mut parser = translator::Parser::create(f);

    let mut buf_writer = BufWriter::new(
        File::create(filename.replace(".vm", ".asm").as_str()).expect("failed to create asm file."),
    );
    let mut code_writer = translator::CodeWriter::create(&mut buf_writer);

    while parser.has_more_commands() {
        parser.advance();
        let command_type = parser.command_type();
        println!("{:?}", command_type);
        code_writer.write_command(&command_type);
    }

    code_writer.flush()
}

fn get_filename(args: &Vec<String>) -> &String {
    if args.len() < 2 {
        panic!("this command needs at least one argument")
    }
    // TODO: needs to read directory too
    if !args[1].ends_with(".vm") {
        panic!("the file is not expected assembly file.")
    }
    &args[1]
}
