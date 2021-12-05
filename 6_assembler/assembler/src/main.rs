use std::env;
mod parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = get_filename(&args);

    let mut parser = parser::Parser::create(filename);
    while parser.has_more_commands() {
        let command_type = parser.command_type();
        println!("{:?}", command_type);

        parser.advance();
    }
}

fn get_filename(args: &Vec<String>) -> &String {
    if args.len() < 2 {
        panic!("this command needs at least one argument")
    }
    &args[1]
}
