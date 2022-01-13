use super::ast::*;
use super::token::*;
use super::tokenizer::*;

#[derive(PartialEq, PartialOrd, Debug)]
enum Priority {
    LOWEST = 0,
    EQUALS = 1,
}

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
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Statement {
        self.advance();

        let identifier = if let TokenType::IDNETIFIER(identifier_token) = &self.cur_token {
            Expression::Identifier(identifier_token.clone())
        } else {
            panic!(
                "failed to get identifier keyword by cur_token: {:?}",
                self.cur_token
            )
        };

        self.advance();

        // TODO: might has [9]
        if !self.current_token_is(TokenType::ASSIGN) {
            panic!("unexpected syntax: {}", self.cur_token.get_xml_tag());
        }

        self.advance();

        let expression = self.parse_expression(Priority::LOWEST);

        while !self.current_token_is_eof_or(TokenType::SEMICOLON) {
            self.advance();
        }

        Statement::LetStatement(identifier, expression)
    }

    fn parse_return_statement(&mut self) -> Statement {
        self.advance();

        let expression = if (self.current_token_is(TokenType::SEMICOLON)) {
            None
        } else {
            Some(self.parse_expression(Priority::LOWEST))
        };

        while !self.current_token_is_eof_or(TokenType::SEMICOLON) {
            self.advance();
        }

        Statement::ReturnStatement(expression)
    }

    fn parse_expression_statement(&mut self) -> Statement {
        let expression = self.parse_expression(Priority::LOWEST);

        while !self.current_token_is_eof_or(TokenType::SEMICOLON) {
            self.advance();
        }
        Statement::ExpressionStatement(expression)
    }

    fn parse_expression(&self, priority: Priority) -> Expression {
        let left_expression = self.parse_prefix_expression(&self.cur_token);
        left_expression
    }

    fn parse_identifier(&self, identifierToken: &IdentifierToken) -> Expression {
        Expression::Identifier(identifierToken.clone())
    }

    fn parse_integer_constant(&self, num: &i32) -> Expression {
        Expression::IntegerConstant(num.clone())
    }

    fn parse_prefix_expression(&self, token: &TokenType) -> Expression {
        match token {
            TokenType::IDNETIFIER(identifierToken) => self.parse_identifier(identifierToken),
            TokenType::NUMBER(num) => self.parse_integer_constant(num),
            _ => panic!(
                "unexpected token is passed to get_prefix_parse_function: {:?}",
                token
            ),
        }
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
        let y = x;
        "
        .as_bytes();
        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse_program();

        match actual {
            Node::Program(statements) => {
                assert_eq!(statements.len(), 2);
                assert_eq!(
                    statements,
                    vec![
                        Statement::LetStatement(
                            Expression::Identifier(IdentifierToken {
                                identifier: "x".to_string(),
                            }),
                            Expression::IntegerConstant(5)
                        ),
                        Statement::LetStatement(
                            Expression::Identifier(IdentifierToken {
                                identifier: "y".to_string(),
                            }),
                            Expression::Identifier(IdentifierToken {
                                identifier: "x".to_string(),
                            })
                        )
                    ]
                );
            }
            _ => panic!("unexpected Node variant"),
        }
    }

    #[test]
    fn return_statements() {
        let source = "
        return 5;
        return x;
        return;
        "
        .as_bytes();
        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse_program();

        match actual {
            Node::Program(statements) => {
                assert_eq!(statements.len(), 3);
                assert_eq!(
                    statements,
                    vec![
                        Statement::ReturnStatement(Some(Expression::IntegerConstant(5))),
                        Statement::ReturnStatement(Some(Expression::Identifier(IdentifierToken {
                            identifier: "x".to_string(),
                        }))),
                        Statement::ReturnStatement(None)
                    ]
                );
            }
            _ => panic!("unexpected Node variant"),
        }
    }

    #[test]
    fn identifier_expression() {
        let source = "
        foobar;
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
                    vec![Statement::ExpressionStatement(Expression::Identifier(
                        IdentifierToken {
                            identifier: "foobar".to_string(),
                        }
                    ))]
                );
            }
            _ => panic!("unexpected Node variant"),
        }
    }

    #[test]
    fn integer_constant_expression() {
        let source = "
        5;
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
                    vec![Statement::ExpressionStatement(Expression::IntegerConstant(
                        5
                    ))]
                );
            }
            _ => panic!("unexpected Node variant"),
        }
    }
}
