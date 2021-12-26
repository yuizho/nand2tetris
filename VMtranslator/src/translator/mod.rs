use std::fs::File;
use std::io::Read;
use std::io::{BufWriter, Write};

mod command_type;

pub struct Parser {
    instructions: Vec<String>,
    current_line: usize,
}

impl Parser {
    // TODO: use BufReader
    pub fn create(f: &mut File) -> Self {
        let mut instructions = String::new();
        f.read_to_string(&mut instructions)
            .expect("filed to read file");

        // TODO: use logger
        println!("{}", instructions);

        Parser {
            instructions: instructions
                .split("\n")
                .map(|s| s.trim().to_string())
                .filter(|s| !s.starts_with("//") && s.len() > 0)
                .collect(),
            current_line: 0,
        }
    }

    pub fn has_more_commands(&self) -> bool {
        self.instructions.len() > self.current_line
    }

    pub fn advance(&mut self) {
        self.current_line += 1
    }

    pub fn command_type(&self) -> command_type::CommandType {
        command_type::CommandType::from_command(&self.instructions[self.current_line])
    }
}

pub struct CodeWriter {
    buf_writer: BufWriter<File>,
}

impl CodeWriter {
    // TODO: wants to pass BufWriter
    pub fn create(file_name: &str) -> Self {
        CodeWriter {
            buf_writer: BufWriter::new(
                File::create(file_name).expect("failed to create asm file."),
            ),
        }
    }

    pub fn write_command(&mut self, command_type: &command_type::CommandType) {
        let instruction = command_type.to_binary_code();
        self.buf_writer
            .write(instruction.as_bytes())
            .expect("failed to write hack file");
    }

    pub fn flush(&mut self) {
        self.buf_writer.flush().expect("failed to write asm file");
    }
}
