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

    pub fn command_type(&self, vm_name: &str) -> command_type::CommandType {
        let command = &self.current_command;

        if command.trim().starts_with("//") || command.trim().is_empty() {
            return command_type::CommandType::Blank;
        }

        command_type::CommandType::from_command(&command, vm_name)
    }
}

pub struct CodeWriter<'a, T: Write> {
    buf_writer: &'a mut BufWriter<T>,
}

impl<'a, T: Write> CodeWriter<'a, T> {
    pub fn create(buf_writer: &'a mut BufWriter<T>) -> Self {
        CodeWriter {
            buf_writer: buf_writer,
        }
    }

    pub fn write_init(&mut self) {
        let init_commands = format!(
            "{}\n{}\n",
            "@256\nD=A\n@SP\nM=D",
            command_type::CommandType::Call("Sys.init".to_string(), 0).to_assembly_code()
        );
        self.buf_writer
            .write(init_commands.as_bytes())
            .expect("failed to write init commands");
    }

    pub fn write_command(&mut self, command_type: &command_type::CommandType) {
        let instruction = command_type.to_assembly_code();
        self.buf_writer
            .write(instruction.as_bytes())
            .expect("failed to write asm file");
    }
}

#[cfg(test)]
mod tests {
    use crate::translator::*;
    use std::io::Cursor;

    #[test]
    fn read() {
        // given
        let data = "push constant 7
        push constant 8
        add"
        .as_bytes();

        // when
        let mut parser = Parser::create(Cursor::new(&data));
        let mut actual = Vec::new();
        while parser.has_more_commands() {
            parser.advance();
            actual.push(parser.command_type("test"));
        }

        // then
        assert_eq!(actual.len(), 4);
        assert_eq!(
            actual.get(0).unwrap(),
            &command_type::CommandType::CPush(command_type::Segment::Constant, 7)
        );
        assert_eq!(
            actual.get(1).unwrap(),
            &command_type::CommandType::CPush(command_type::Segment::Constant, 8)
        );
        assert_eq!(
            actual.get(2).unwrap(),
            &command_type::CommandType::BinaryArithmetic(command_type::BinaryArithmetic::Add)
        );
        assert_eq!(actual.get(3).unwrap(), &command_type::CommandType::Blank);
    }

    #[test]
    fn write() {
        // given
        let cursor = Cursor::new(Vec::new());
        let mut buf_writer = BufWriter::new(cursor);

        // when
        let mut writer = CodeWriter::create(&mut buf_writer);
        writer.write_command(&command_type::CommandType::CPush(
            command_type::Segment::Constant,
            7,
        ));
        writer.write_command(&command_type::CommandType::BinaryArithmetic(
            command_type::BinaryArithmetic::And,
        ));
        writer.write_command(&command_type::CommandType::Blank);
        buf_writer.flush().unwrap();

        // then
        let actual_bytes = buf_writer.into_inner().unwrap().into_inner();
        let actual = actual_bytes.iter().map(|&s| s as char).collect::<String>();
        assert_eq!(
            actual,
            "@7\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M-1\nA=M\nM=D&M\n@SP\nM=M+1\n"
        );
    }
}
