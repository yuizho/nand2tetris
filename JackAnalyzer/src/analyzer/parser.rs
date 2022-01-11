use super::ast::*;
use super::token::*;
use super::tokenizer::*;

pub struct Parser<'a> {
    tokenizer: &'a mut JackTokenizer,
    cur_token: TokenType,
}
impl<'a> Parser<'a> {
    pub fn new(tokenizer: &'a mut JackTokenizer) -> Self {
        let cur_token = tokenizer.advance();
        Parser {
            tokenizer,
            cur_token,
        }
    }

    pub fn advance(&mut self) {
        self.cur_token = self.tokenizer.advance()
    }

    pub fn parse_program(&mut self) -> Node {
        let mut statements = vec![];
        while self.tokenizer.has_more_tokens() {
            statements.push(self.parse_statement());
            self.advance()
        }
        Node::Program(statements)
    }

    fn parse_statement(&mut self) -> Statement {
        match &self.cur_token {
            TokenType::KEYWORD(keyword) => match keyword {
                Keyword::LET => self.parse_let_statement(),
                Keyword::RETURN => self.parse_return_statement(),
                _ => panic!(
                    "unexpected token to parse statement(keyword): {}",
                    self.cur_token.get_xml_tag()
                ),
            },
            _ => panic!(
                "unexpected token to parse statement: {}",
                self.cur_token.get_xml_tag()
            ),
        }
    }

    fn parse_let_statement(&mut self) -> Statement {
        self.advance();

        let identifier = Expression::Identifier(self.cur_token.clone());

        self.advance();

        // TODO: might has [9]
        if !self.current_token_is(TokenType::ASSIGN) {
            panic!("unexpected syntax: {}", self.cur_token.get_xml_tag());
        }

        self.advance();

        while !self.current_token_is_eof_or(TokenType::SEMICOLON) {
            self.advance();
        }

        Statement::LetStatement(
            TokenType::KEYWORD(Keyword::LET),
            identifier,
            Expression::Dummy,
        )
    }

    fn parse_return_statement(&mut self) -> Statement {
        self.advance();
        while !self.current_token_is_eof_or(TokenType::SEMICOLON) {
            self.advance();
        }

        Statement::ReturnStatement(TokenType::KEYWORD(Keyword::RETURN), Some(Expression::Dummy))
    }

    fn current_token_is(&self, token: TokenType) -> bool {
        self.cur_token == token
    }

    fn current_token_is_eof_or(&self, token: TokenType) -> bool {
        self.current_token_is(token) || self.cur_token == TokenType::EOF
    }
}

#[cfg(test)]
mod tests {
    use crate::analyzer::ast::*;
    use crate::analyzer::parser::*;
    use crate::analyzer::token::*;
    use crate::analyzer::tokenizer::*;
    use std::io::Cursor;

    #[test]
    fn let_statements() {
        let source = "
        let x = 5;
        "
        .as_bytes();
        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse_program();

        match actual {
            Node::Program(statements) => {
                assert_eq!(statements.len(), 1);
                assert_eq!(
                    statements,
                    vec![Statement::LetStatement(
                        TokenType::KEYWORD(Keyword::LET),
                        Expression::Identifier(TokenType::IDNETIFIER("x".to_string())),
                        Expression::Dummy
                    )]
                );
            }
            _ => panic!("unexpected Node variant"),
        }
    }

    #[test]
    fn return_statements() {
        let source = "
        return 5
        "
        .as_bytes();
        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse_program();

        match actual {
            Node::Program(statements) => {
                assert_eq!(statements.len(), 1);
                assert_eq!(
                    statements,
                    vec![Statement::ReturnStatement(
                        TokenType::KEYWORD(Keyword::RETURN),
                        Some(Expression::Dummy)
                    )]
                );
            }
            _ => panic!("unexpected Node variant"),
        }
    }
}
