use super::ast::*;
use super::token::*;
use super::tokenizer::*;

pub struct Parser<'a> {
    tokenizer: &'a mut JackTokenizer,
    cur_token: TokenType,
    peek_token: TokenType,
}
impl<'a> Parser<'a> {
    pub fn new(tokenizer: &'a mut JackTokenizer) -> Self {
        let cur_token = tokenizer.advance();
        let peek_token = tokenizer.advance();
        Parser {
            tokenizer,
            cur_token,
            peek_token,
        }
    }

    pub fn advance(&mut self) {
        // TODO: should use reference
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.tokenizer.advance();
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
            let expression = self.parse_expression();
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

        let expression = self.parse_expression();

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
            Some(self.parse_expression())
        };

        while !self.current_token_is_eof_or(TokenType::SEMICOLON) {
            self.advance();
        }

        Statement::ReturnStatement(expression)
    }

    fn parse_expression_statement(&mut self) -> Statement {
        let expression = self.parse_expression();

        while !self.current_token_is_eof_or(TokenType::SEMICOLON) {
            self.advance();
        }
        Statement::ExpressionStatement(expression)
    }

    fn parse_expression(&mut self) -> Expression {
        let left_term = self.parse_term();

        // TODO: op precedence is not implemented
        match self.parse_infix_expression() {
            Some(binary_op) => {
                self.advance();
                Expression::new_binary_op(left_term, binary_op)
            }
            None => Expression::new(left_term),
        }
    }

    fn parse_identifier(&self, identifier_token: &IdentifierToken) -> Term {
        Term::Identifier(identifier_token.clone())
    }

    fn parse_integer_constant(&self, num: &i32) -> Term {
        Term::IntegerConstant(num.clone())
    }

    fn parse_string_constant(&self, str_value: &String) -> Term {
        Term::StringConstant(str_value.clone())
    }

    fn parse_unary_op_constant(&mut self) -> Term {
        let op = self.cur_token.clone();

        self.advance();

        let term = self.parse_term();

        Term::UnaryOp(UnaryOpToken::new(op), Box::new(term))
    }

    fn parse_term(&mut self) -> Term {
        let token = &self.cur_token;
        match token {
            TokenType::IDNETIFIER(identifier_token) => self.parse_identifier(identifier_token),
            TokenType::NUMBER(num) => self.parse_integer_constant(num),
            TokenType::STRING(str_value) => self.parse_string_constant(str_value),
            TokenType::MINUS | TokenType::TILDE => self.parse_unary_op_constant(),
            _ => panic!(
                "unexpected token is passed to get_prefix_parse_function: {:?}",
                token
            ),
        }
    }

    fn parse_infix_expression(&mut self) -> Option<BinaryOp> {
        if BinaryOpToken::is_binary_op_token_type(&self.peek_token) {
            self.advance();
            let op = BinaryOpToken::new(self.cur_token.clone());

            self.advance();

            let right_term = self.parse_term();

            Some(BinaryOp::new(op, right_term))
        } else {
            None
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
                    IdentifierToken::new("x".to_string()),
                    None,
                    Expression::new(Term::IntegerConstant(5))
                ),
                Statement::LetStatement(
                    IdentifierToken::new("y".to_string()),
                    None,
                    Expression::new(Term::Identifier(IdentifierToken::new("x".to_string())))
                ),
                Statement::LetStatement(
                    IdentifierToken::new("z".to_string()),
                    Some(Expression::new(Term::Identifier(IdentifierToken::new(
                        "i".to_string()
                    )))),
                    Expression::new(Term::Identifier(IdentifierToken::new("y".to_string()))),
                ),
                Statement::LetStatement(
                    IdentifierToken::new("z".to_string()),
                    Some(Expression::new(Term::IntegerConstant(0))),
                    Expression::new(Term::Identifier(IdentifierToken::new("y".to_string())))
                ),
                Statement::LetStatement(
                    IdentifierToken::new("z".to_string()),
                    None,
                    Expression::new(Term::StringConstant("foo".to_string()))
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
                Statement::ReturnStatement(Some(Expression::new(Term::IntegerConstant(5)))),
                Statement::ReturnStatement(Some(Expression::new(Term::Identifier(
                    IdentifierToken::new("x".to_string(),)
                )))),
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
            vec![Statement::ExpressionStatement(Expression::new(
                Term::Identifier(IdentifierToken::new("foobar".to_string(),))
            ))]
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
            vec![Statement::ExpressionStatement(Expression::new(
                Term::IntegerConstant(5)
            ))]
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
            vec![Statement::ExpressionStatement(Expression::new(
                Term::StringConstant("str value!!".to_string())
            ))]
        );
    }

    #[test]
    fn unary_op_expression() {
        let source = "
        -1;
        ~i;
        "
        .as_bytes();
        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse_program();

        assert_eq!(actual.statements.len(), 2);
        assert_eq!(
            actual.statements,
            vec![
                Statement::ExpressionStatement(Expression::new(Term::UnaryOp(
                    UnaryOpToken::new(TokenType::MINUS),
                    Box::new(Term::IntegerConstant(1))
                ))),
                Statement::ExpressionStatement(Expression::new(Term::UnaryOp(
                    UnaryOpToken::new(TokenType::TILDE),
                    Box::new(Term::Identifier(IdentifierToken::new("i".to_string(),)))
                ))),
            ]
        );
    }

    #[test]
    fn bianry_op_expression() {
        let source = "
        1 + i;
        2 > 3;
        "
        .as_bytes();
        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse_program();

        assert_eq!(actual.statements.len(), 2);
        assert_eq!(
            actual.statements,
            vec![
                Statement::ExpressionStatement(Expression::new_binary_op(
                    Term::IntegerConstant(1),
                    BinaryOp::new(
                        BinaryOpToken::new(TokenType::PLUS),
                        Term::Identifier(IdentifierToken::new("i".to_string()))
                    )
                )),
                Statement::ExpressionStatement(Expression::new_binary_op(
                    Term::IntegerConstant(2),
                    BinaryOp::new(BinaryOpToken::new(TokenType::GT), Term::IntegerConstant(3),)
                ))
            ]
        );
    }
}
