use std::env;
use std::fs::File;
use std::io::Read;
mod parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = get_filename(&args);

    let mut f = File::open(filename).expect("file not found");
    parser::Parser::create(&mut f);
}

fn get_filename(args: &Vec<String>) -> &String {
    if args.len() < 2 {
        panic!("this command needs at least one argument")
    }
    // TODO: needs to read directory
    if !args[1].ends_with(".vm") {
        panic!("the file is not expected assembly file.")
    }
    &args[1]
}
