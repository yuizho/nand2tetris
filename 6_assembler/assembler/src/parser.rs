use std::fs::File;
use std::io::Read;

pub struct Parser {
    contents: Vec<String>,
    current_line: usize,
}

impl Parser {
    pub fn create(filename: &String) -> Self {
        let mut contents = String::new();
        let mut f = File::open(filename).expect("file not found");
        f.read_to_string(&mut contents).expect("filed to read file");
        Parser {
            contents: contents.split("\n").map(|s| s.to_string()).collect(),
            current_line: 0,
        }
    }

    pub fn has_more_commands(&self) -> bool {
        self.contents.len() > self.current_line
    }

    pub fn advance(&mut self) {
        self.current_line += 1
    }

    pub fn command_type(&self) -> CommandType {
        CommandType::instruction_of(&self.contents[self.current_line])
    }
}

#[derive(Debug)]
pub enum CommandType {
    A(usize),
    C(Option<String>, String, Option<String>),
    L(usize),
    Blank,
}

impl CommandType {
    // TODO: refactor
    fn instruction_of(instruction: &String) -> Self {
        let trimed = instruction.trim();
        if trimed.is_empty() || trimed.starts_with("//") {
            CommandType::Blank
        } else if trimed.starts_with("@") {
            CommandType::A(
                trimed[1..]
                    .parse::<usize>()
                    .expect("symbol doesn't implement yet"),
            )
        } else if trimed.starts_with("(") {
            // TODO
            CommandType::Blank
        } else if trimed.contains("=") {
            let (dest, cmp) = trimed.split_once('=').unwrap();
            CommandType::C(Some(String::from(dest)), String::from(cmp), None)
        } else if trimed.contains(";") {
            let (cmp, jmp) = trimed.split_once(';').unwrap();
            CommandType::C(None, String::from(cmp), Some(String::from(jmp)))
        } else {
            panic!("unexpected syntax {}", instruction)
        }
    }
}
