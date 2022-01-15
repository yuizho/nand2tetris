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

    pub fn parse_program(&mut self) -> Program {
        let mut statements = vec![];
        while self.tokenizer.has_more_tokens() {
            statements.push(self.parse_statement());
            self.advance()
        }
        Program { statements }
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

        let identifier_token = if let TokenType::IDNETIFIER(identifier_token) = &self.cur_token {
            identifier_token.clone()
        } else {
            panic!(
                "failed to get identifier keyword of let: {:?}",
                self.cur_token
            )
        };

        self.advance();

        let index_expression = if self.current_token_is(TokenType::LBRACKET) {
            self.advance();
            let expression = self.parse_expression(Priority::LOWEST);
            self.advance();
            if !self.current_token_is(TokenType::RBRACKET) {
                panic!("unexpected syntax of let: {}", self.cur_token.get_xml_tag());
            }
            self.advance();
            Some(expression)
        } else {
            None
        };

        if !self.current_token_is(TokenType::ASSIGN) {
            panic!("unexpected syntax of let: {}", self.cur_token.get_xml_tag());
        }

        self.advance();

        let expression = self.parse_expression(Priority::LOWEST);

        while !self.current_token_is_eof_or(TokenType::SEMICOLON) {
            self.advance();
        }

        Statement::LetStatement(identifier_token, index_expression, expression)
    }

    fn parse_return_statement(&mut self) -> Statement {
        self.advance();

        let expression = if self.current_token_is(TokenType::SEMICOLON) {
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

    fn parse_identifier(&self, identifier_token: &IdentifierToken) -> Expression {
        Expression {
            left_term: Term::Identifier(identifier_token.clone()),
            binary_op: None,
        }
    }

    fn parse_integer_constant(&self, num: &i32) -> Expression {
        Expression {
            left_term: Term::IntegerConstant(num.clone()),
            binary_op: None,
        }
    }

    fn parse_string_constant(&self, str_value: &String) -> Expression {
        Expression {
            left_term: Term::StringConstant(str_value.clone()),
            binary_op: None,
        }
    }

    fn parse_prefix_expression(&self, token: &TokenType) -> Expression {
        match token {
            TokenType::IDNETIFIER(identifier_token) => self.parse_identifier(identifier_token),
            TokenType::NUMBER(num) => self.parse_integer_constant(num),
            TokenType::STRING(str_value) => self.parse_string_constant(str_value),
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
    use crate::analyzer::parser::*;
    use std::io::Cursor;

    #[test]
    fn let_statements() {
        let source = "
        let x = 5;
        let y = x;
        let z[i] = y;
        let z[0] =  y;
        let z = \"foo\";
        "
        .as_bytes();
        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse_program();

        assert_eq!(actual.statements.len(), 5);
        assert_eq!(
            actual.statements,
            vec![
                Statement::LetStatement(
                    IdentifierToken {
                        identifier: "x".to_string(),
                    },
                    None,
                    Expression {
                        left_term: Term::IntegerConstant(5),
                        binary_op: None
                    }
                ),
                Statement::LetStatement(
                    IdentifierToken {
                        identifier: "y".to_string(),
                    },
                    None,
                    Expression {
                        left_term: Term::Identifier(IdentifierToken {
                            identifier: "x".to_string(),
                        }),
                        binary_op: None,
                    }
                ),
                Statement::LetStatement(
                    IdentifierToken {
                        identifier: "z".to_string(),
                    },
                    Some(Expression {
                        left_term: Term::Identifier(IdentifierToken {
                            identifier: "i".to_string(),
                        }),
                        binary_op: None,
                    }),
                    Expression {
                        left_term: Term::Identifier(IdentifierToken {
                            identifier: "y".to_string(),
                        }),
                        binary_op: None,
                    }
                ),
                Statement::LetStatement(
                    IdentifierToken {
                        identifier: "z".to_string(),
                    },
                    Some(Expression {
                        left_term: Term::IntegerConstant(0),
                        binary_op: None
                    }),
                    Expression {
                        left_term: Term::Identifier(IdentifierToken {
                            identifier: "y".to_string(),
                        }),
                        binary_op: None
                    }
                ),
                Statement::LetStatement(
                    IdentifierToken {
                        identifier: "z".to_string(),
                    },
                    None,
                    Expression {
                        left_term: Term::StringConstant("foo".to_string()),
                        binary_op: None
                    }
                ),
            ]
        );
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

        assert_eq!(actual.statements.len(), 3);
        assert_eq!(
            actual.statements,
            vec![
                Statement::ReturnStatement(Some(Expression {
                    left_term: Term::IntegerConstant(5),
                    binary_op: None
                })),
                Statement::ReturnStatement(Some(Expression {
                    left_term: Term::Identifier(IdentifierToken {
                        identifier: "x".to_string(),
                    }),
                    binary_op: None
                })),
                Statement::ReturnStatement(None)
            ]
        );
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

        assert_eq!(actual.statements.len(), 1);
        assert_eq!(
            actual.statements,
            vec![Statement::ExpressionStatement(Expression {
                left_term: Term::Identifier(IdentifierToken {
                    identifier: "foobar".to_string(),
                }),
                binary_op: None
            }),]
        );
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

        assert_eq!(actual.statements.len(), 1);
        assert_eq!(
            actual.statements,
            vec![Statement::ExpressionStatement(Expression {
                left_term: Term::IntegerConstant(5),
                binary_op: None,
            })]
        );
    }

    #[test]
    fn string_constant_expression() {
        let source = "
        \"str value!!\";
        "
        .as_bytes();
        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse_program();

        assert_eq!(actual.statements.len(), 1);
        assert_eq!(
            actual.statements,
            vec![Statement::ExpressionStatement(Expression {
                left_term: Term::StringConstant("str value!!".to_string()),
                binary_op: None
            })]
        );
    }
}
