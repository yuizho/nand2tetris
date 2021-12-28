use std::fs::File;
use std::io::Read;
use std::io::{BufRead, BufReader, BufWriter, Seek, Write};

mod command_type;

pub struct Parser<T: Read + Seek> {
    buf_reader: BufReader<T>,
    current_command: String,
    reached_to_eof: bool,
}

impl<T: Read + Seek> Parser<T> {
    pub fn create(read: T) -> Self {
        Parser {
            buf_reader: BufReader::new(read),
            current_command: "".to_string(),
            reached_to_eof: false,
        }
    }

    pub fn has_more_commands(&self) -> bool {
        !self.reached_to_eof
    }

    pub fn advance(&mut self) {
        let command = &mut String::new();
        // If this function returns [`Ok(0)`], the stream has reached EOF.
        let reached_to_eof = self
            .buf_reader
            .read_line(command)
            .expect("failed current_command line")
            == 0;
        self.current_command = command.to_string();
        self.reached_to_eof = reached_to_eof
    }

    pub fn command_type(&self) -> command_type::CommandType {
        let command = &self.current_command;

        if command.trim().starts_with("//") || command.trim().is_empty() {
            return command_type::CommandType::Blank;
        }

        command_type::CommandType::from_command(&command)
    }
}

pub struct CodeWriter<T: Write> {
    buf_writer: BufWriter<T>,
}

impl<T: Write> CodeWriter<T> {
    pub fn create(writer: T) -> Self {
        CodeWriter {
            buf_writer: BufWriter::new(writer),
        }
    }

    pub fn write_command(&mut self, command_type: &command_type::CommandType) {
        let instruction = command_type.to_assembly_code();
        self.buf_writer
            .write(instruction.as_bytes())
            .expect("failed to write hack file");
    }

    pub fn flush(&mut self) {
        self.buf_writer.flush().expect("failed to write asm file");
    }
}
