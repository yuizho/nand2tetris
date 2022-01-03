use std::io::{BufReader, Read};

use super::token;

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

    pub fn advance(&mut self) -> token::TokenType {
        self.skip_whitespace();

        let token = match self.current_char {
            '{' => token::TokenType::LBRACE,
            '}' => token::TokenType::RBRACE,
            '(' => token::TokenType::LPAREN,
            ')' => token::TokenType::RPAREN,
            '[' => token::TokenType::LBRACKET,
            ']' => token::TokenType::RBRACKET,
            '.' => token::TokenType::DOT,
            ',' => token::TokenType::COMMA,
            ';' => token::TokenType::SEMICOLON,
            '+' => token::TokenType::PLUS,
            '-' => token::TokenType::MINUS,
            '*' => token::TokenType::ASTERISK,
            '/' => token::TokenType::SLASH,
            '&' => token::TokenType::AND,
            '|' => token::TokenType::OR,
            '<' => token::TokenType::LT,
            '>' => token::TokenType::GT,
            '=' => token::TokenType::ASSIGN,
            '~' => token::TokenType::TILDE,
            _ if self.is_letter() => token::TokenType::lookup_identify(&self.read_identifier()),
            _ if self.is_digit() => token::TokenType::NUMBER(self.read_number()),
            c => panic!("unexpected token: {}", c),
        };

        self.read_char();
        token
    }

    fn read_number(&mut self) -> i32 {
        let start_position = self.position;
        while self.is_letter() {
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
            panic!("the read position reached to EOF.");
        }
        self.current_char = self.input[self.read_position];

        self.position = self.read_position;
        self.read_position += 1;
    }
}
