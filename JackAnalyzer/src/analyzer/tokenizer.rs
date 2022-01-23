use std::io::{BufReader, Read};

use super::token;

const EMPTY_CHAR: char = 0 as char;

pub struct JackTokenizer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    current_char: char,
}

impl JackTokenizer {
    pub fn new<T: Read>(read: T) -> Self {
        let mut buf_reader = BufReader::new(read);
        let mut readed = String::new();
        buf_reader
            .read_to_string(&mut readed)
            .expect("failed to read file");
        let chars = readed.chars().collect::<Vec<char>>();
        let first_char = chars[0];

        JackTokenizer {
            input: chars,
            position: 0,
            read_position: 1,
            current_char: first_char,
        }
    }

    pub fn has_more_tokens(&self) -> bool {
        self.input.len() > self.read_position
    }

    pub fn peek(&mut self) -> token::TokenType {
        let start_position = self.position;
        let start_read_posision = self.read_position;
        let start_current_char = self.current_char;

        let token = self.advance();

        self.position = start_position;
        self.read_position = start_read_posision;
        self.current_char = start_current_char;

        token
    }

    pub fn advance(&mut self) -> token::TokenType {
        self.skip_whitespace();

        let token = match self.current_char {
            '"' => token::TokenType::String(self.read_string()),
            '{' => token::TokenType::Lbrace,
            '}' => token::TokenType::Rbrace,
            '(' => token::TokenType::Lparen,
            ')' => token::TokenType::Rparen,
            '[' => token::TokenType::Lbracket,
            ']' => token::TokenType::Rbracket,
            '.' => token::TokenType::Dot,
            ',' => token::TokenType::Comma,
            ';' => token::TokenType::Semicolon,
            '+' => token::TokenType::Plus,
            '-' => token::TokenType::Minus,
            '*' => token::TokenType::Asterisk,
            '/' => {
                if self.peek_char() == '/' {
                    self.skip_line_comments();
                    self.read_char();
                    return self.advance();
                } else if self.peek_char() == '*' {
                    self.skip_multi_line_comments();
                    self.read_char();
                    return self.advance();
                } else {
                    token::TokenType::Slash
                }
            }
            '&' => token::TokenType::And,
            '|' => token::TokenType::Or,
            '<' => token::TokenType::Lt,
            '>' => token::TokenType::Gt,
            '=' => token::TokenType::Assign,
            '~' => token::TokenType::Tilde,
            _ if self.is_letter() => {
                return token::TokenType::lookup_identify(&self.read_identifier())
            }
            _ if self.is_digit() => return token::TokenType::Number(self.read_number()),
            c if c == EMPTY_CHAR => token::TokenType::Eof,
            c => panic!("unexpected token: {}", c),
        };

        self.read_char();
        token
    }

    fn skip_line_comments(&mut self) {
        while self.current_char != '\n' {
            self.read_char();
        }
    }

    fn skip_multi_line_comments(&mut self) {
        loop {
            self.read_char();
            if self.current_char == '*' && self.peek_char() == '/' {
                self.read_char();
                break;
            }
        }
    }

    fn read_number(&mut self) -> i32 {
        let start_position = self.position;
        while self.is_digit() {
            self.read_char();
        }
        let readed = self.input[start_position..self.position]
            .iter()
            .collect::<String>();
        readed.parse::<i32>().unwrap()
    }

    fn read_identifier(&mut self) -> String {
        let start_position = self.position;
        while self.is_letter() {
            self.read_char();
        }
        self.input[start_position..self.position].iter().collect()
    }

    fn read_string(&mut self) -> String {
        let start_position = self.position + 1;
        loop {
            self.read_char();
            if self.current_char == '"' {
                break;
            }
        }
        self.input[start_position..self.position].iter().collect()
    }

    fn is_digit(&self) -> bool {
        '0' <= self.current_char && self.current_char <= '9'
    }

    fn is_letter(&self) -> bool {
        'a' <= self.current_char && self.current_char <= 'z'
            || 'A' <= self.current_char && self.current_char <= 'Z'
            || self.current_char == '_'
    }

    fn skip_whitespace(&mut self) {
        while self.current_char == ' '
            || self.current_char == '\t'
            || self.current_char == '\n'
            || self.current_char == '\r'
        {
            self.read_char()
        }
    }

    fn read_char(&mut self) {
        if !self.has_more_tokens() {
            self.current_char = EMPTY_CHAR;
        } else {
            self.current_char = self.input[self.read_position];

            self.position = self.read_position;
            self.read_position += 1;
        }
    }

    fn peek_char(&mut self) -> char {
        if self.has_more_tokens() {
            self.input[self.read_position]
        } else {
            EMPTY_CHAR
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::analyzer::token::*;
    use crate::analyzer::tokenizer::*;
    use std::io::Cursor;

    #[test]
    fn read_simple_class() {
        let source = "class Main {\nstatic boolean test;\n }\n".as_bytes();
        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut actual: Vec<TokenType> = Vec::new();
        while tokenizer.has_more_tokens() {
            actual.push(tokenizer.advance());
        }

        assert_eq!(
            actual,
            vec![
                TokenType::Keyword(Keyword::Class),
                TokenType::Identifier(IdentifierToken::new("Main",)),
                TokenType::Lbrace,
                TokenType::Keyword(Keyword::Static),
                TokenType::Keyword(Keyword::Boolean),
                TokenType::Identifier(IdentifierToken::new("test",)),
                TokenType::Semicolon,
                TokenType::Rbrace,
            ]
        );
    }

    #[test]
    fn read_source_which_has_comments() {
        let source =
            "class Main {\n// comments\nstatic boolean test; //comments\n/* comments\nmulti line */\n }\n"
                .as_bytes();
        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut actual: Vec<TokenType> = Vec::new();
        while tokenizer.has_more_tokens() {
            actual.push(tokenizer.advance());
        }

        assert_eq!(
            actual,
            vec![
                TokenType::Keyword(Keyword::Class),
                TokenType::Identifier(IdentifierToken::new("Main",)),
                TokenType::Lbrace,
                TokenType::Keyword(Keyword::Static),
                TokenType::Keyword(Keyword::Boolean),
                TokenType::Identifier(IdentifierToken::new("test",)),
                TokenType::Semicolon,
                TokenType::Rbrace,
            ]
        );
    }
}
