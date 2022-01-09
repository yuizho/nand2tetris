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
        match self.cur_token {
            TokenType::KEYWORD(Keyword::LET) => self.parse_let_statement(),
            _ => panic!(
                "unexpected token to parse statement: {}",
                self.cur_token.get_literal()
            ),
        }
    }

    fn parse_let_statement(&mut self) -> Statement {
        self.advance();

        let identifier = Expression::Identifier(self.cur_token.clone());

        self.advance();

        if self.cur_token != TokenType::ASSIGN {
            panic!("unexpected syntax: {}", self.cur_token.get_literal());
        }

        self.advance();

        while self.cur_token != TokenType::SEMICOLON {
            self.advance();
        }

        Statement::LetStatement(
            TokenType::KEYWORD(Keyword::LET),
            identifier,
            Expression::Dummy,
        )
    }

    fn is_current_token(&self, token: TokenType) -> bool {
        self.cur_token == token
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
}
