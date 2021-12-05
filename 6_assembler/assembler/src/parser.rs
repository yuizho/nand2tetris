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

pub fn convert_code(command_type: CommandType) -> Option<i32> {
    match command_type {
        CommandType::A(symbol) => Some(symbol as i32),
        CommandType::C(dest, cmp, jmp) => {
            Some(0b1110000000000000 + convert_comp(cmp) + convert_dest(&dest) + convert_jump(&jmp))
        }
        // TODO: L
        _ => None,
    }
}

fn convert_comp(cmp: String) -> i32 {
    let trimed_cmp = cmp.replace(" ", "");
    match trimed_cmp.as_str() {
        "0" => 0b0101010000000,
        "1" => 0b0111111000000,
        "-1" => 0b0111010000000,
        "D" => 0b0001100000000,
        "A" => 0b0110000000000,
        "!D" => 0b0001101000000,
        "!A" => 0b0110001000000,
        "-D" => 0b0001111000000,
        "-A" => 0b0110011000000,
        "D+1" => 0b0011111000000,
        "A+1" => 0b0110111000000,
        "D-1" => 0b0001110000000,
        "A-1" => 0b0110010000000,
        "D+A" => 0b0000010000000,
        "D-A" => 0b0010011000000,
        "A-D" => 0b0000111000000,
        "D&A" => 0b0000000000000,
        "D|A" => 0b0010101000000,
        "M" => 0b1110000000000,
        "!M" => 0b1110001000000,
        "-M" => 0b1110011000000,
        "M+1" => 0b1110111000000,
        "M-1" => 0b1110010000000,
        "D+M" => 0b1000010000000,
        "D-M" => 0b1010011000000,
        "M-D" => 0b1000111000000,
        "D&M" => 0b1000000000000,
        "D|M" => 0b1010101000000,
        _ => panic!("unexpected cmp code is comming: {}", cmp),
    }
}

fn convert_dest(dest: &Option<String>) -> i32 {
    match dest.as_deref() {
        Some("M") => 0b001000,
        Some("D") => 0b010000,
        Some("MD") => 0b011000,
        Some("A") => 0b100000,
        Some("AM") => 0b101000,
        Some("AD") => 0b110000,
        Some("AMD") => 0b111000,
        None => 0b000000,
        Some(a) => panic!("unexpected dest code is comming: {}", a),
    }
}

fn convert_jump(jmp: &Option<String>) -> i32 {
    match jmp.as_deref() {
        Some("JGT") => 0b001,
        Some("JEQ") => 0b010,
        Some("JGE") => 0b011,
        Some("JLT") => 0b100,
        Some("JNE") => 0b101,
        Some("JLE") => 0b110,
        Some("JMP") => 0b111,
        None => 0b000,
        Some(a) => panic!("unexpected jump code is comming: {}", a),
    }
}
