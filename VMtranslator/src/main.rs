use std::env;
use std::fs::File;

mod translator;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = get_filename(&args);

    let mut f = File::open(filename).expect("file not found");
    let mut parser = translator::Parser::create(&mut f);
    let mut code_writer = translator::CodeWriter::create(filename.replace(".vm", ".asm").as_str());

    while parser.has_more_commands() {
        let command_type = parser.command_type();
        code_writer.write_command(&command_type);
        parser.advance()
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
