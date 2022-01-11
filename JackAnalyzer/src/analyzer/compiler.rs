use super::token::TokenType;
use std::io::{BufWriter, Write};

pub struct CompilationEngine<'a, T: Write> {
    buf_writer: &'a mut BufWriter<T>,
}

impl<'a, T: Write> CompilationEngine<'a, T> {
    pub fn new(buf_writer: &'a mut BufWriter<T>) -> Self {
        CompilationEngine {
            buf_writer: buf_writer,
        }
    }

    pub fn compile(&mut self, token: TokenType) {
        let tag = match token {
            TokenType::KEYWORD(keyword) => {
                format!("{}\n", keyword.get_xml_tag())
            }
            TokenType::IDNETIFIER(identifier) => {
                format!("{}\n", identifier)
            }
            TokenType::STRING(str) => format!("{}\n", str),
            TokenType::NUMBER(num) => format!("{}\n", num),
            TokenType::COMMENTS => "".to_string(),
            TokenType::EOF => "".to_string(),
            symbol => format!("{}\n", symbol.get_xml_tag()),
        };
        self.buf_writer
            .write(tag.as_bytes())
            .expect("failed to write tags");
    }
}
