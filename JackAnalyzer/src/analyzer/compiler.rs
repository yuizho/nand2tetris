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
                format!("<keyword> {} </keyword>\n", keyword.get_literal())
            }
            TokenType::IDNETIFIER(identifier) => {
                format!("<identifier> {} </identifier>\n", identifier.to_string())
            }
            TokenType::STRING(str) => format!("<stringConstant> {} </stringConstant>\n", str),
            TokenType::NUMBER(num) => format!("<integerConstant> {} </integerConstant>\n", num),
            TokenType::COMMENTS => "".to_string(),
            TokenType::EOF => "".to_string(),
            symbol => format!("<symbol> {} </symbol>\n", symbol.get_literal()),
        };
        self.buf_writer
            .write(tag.as_bytes())
            .expect("failed to write tags");
    }
}
