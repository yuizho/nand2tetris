use std::fs::File;
use std::io::Read;

pub struct Parser {
    instructions: String,
    currentCommand: CommandType,
}

impl Parser {
    pub fn create(f: &mut File) {
        let mut contents = String::new();
        f.read_to_string(&mut contents).expect("filed to read file");

        println!("{}", contents);
    }

    pub fn has_more_commands(&self) -> bool {
        true
    }

    pub fn advance() {}

    pub fn command_type() -> CommandType {
        CommandType::CArithmetic
    }
}

pub enum CommandType {
    CArithmetic,
    CPush,
}
