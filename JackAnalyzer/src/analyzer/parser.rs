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
        self.cur_token = self.tokenizer.advance();
    }

    pub fn peek_token(&mut self) -> TokenType {
        self.tokenizer.peek()
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
            TokenType::Keyword(Keyword::Let) => self.parse_let_statement(),
            TokenType::Keyword(Keyword::Return) => self.parse_return_statement(),
            TokenType::Keyword(Keyword::While) => self.parse_while_statement(),
            TokenType::Keyword(Keyword::If) => self.parse_if_statement(),
            TokenType::Keyword(Keyword::Do) => self.parse_do_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Statement {
        self.advance();

        let identifier_token = if let TokenType::Identifier(identifier_token) = &self.cur_token {
            identifier_token.clone()
        } else {
            panic!(
                "failed to get identifier keyword of let: {:?}",
                self.cur_token
            )
        };

        self.advance();

        let index_expression = if self.current_token_is(TokenType::Lbracket) {
            self.advance();
            let expression = self.parse_expression();
            self.advance();
            if !self.current_token_is(TokenType::Rbracket) {
                panic!("unexpected syntax of let: {}", self.cur_token.get_xml_tag());
            }
            self.advance();
            Some(expression)
        } else {
            None
        };

        if !self.current_token_is(TokenType::Assign) {
            panic!("unexpected syntax of let: {}", self.cur_token.get_xml_tag());
        }

        self.advance();

        let expression = self.parse_expression();

        while !self.current_token_is_eof_or(TokenType::Semicolon) {
            self.advance();
        }

        Statement::Let(identifier_token, index_expression, expression)
    }

    fn parse_return_statement(&mut self) -> Statement {
        self.advance();

        let expression = if self.current_token_is(TokenType::Semicolon) {
            None
        } else {
            Some(self.parse_expression())
        };

        while !self.current_token_is_eof_or(TokenType::Semicolon) {
            self.advance();
        }

        Statement::Return(expression)
    }

    fn parse_while_statement(&mut self) -> Statement {
        self.advance();
        if !self.current_token_is(TokenType::Lparen) {
            panic!(
                "unexpected syntax of while: {}",
                self.cur_token.get_xml_tag()
            );
        }

        self.advance();
        let expression = self.parse_expression();

        self.advance();
        if !self.current_token_is(TokenType::Rparen) {
            panic!(
                "unexpected syntax of while: {}",
                self.cur_token.get_xml_tag()
            );
        }

        self.advance();
        if !self.current_token_is(TokenType::Lbrace) {
            panic!(
                "unexpected syntax of while: {}",
                self.cur_token.get_xml_tag()
            );
        }

        self.advance();
        let mut statements = vec![];
        while !self.current_token_is_eof_or(TokenType::Rbrace) {
            statements.push(self.parse_statement());
            self.advance();
        }

        Statement::While(expression, statements)
    }

    fn parse_if_statement(&mut self) -> Statement {
        self.advance();
        if !self.current_token_is(TokenType::Lparen) {
            panic!("unexpected syntax of if: {}", self.cur_token.get_xml_tag());
        }

        self.advance();
        let expression = self.parse_expression();

        self.advance();
        if !self.current_token_is(TokenType::Rparen) {
            panic!("unexpected syntax of if: {}", self.cur_token.get_xml_tag());
        }

        self.advance();
        if !self.current_token_is(TokenType::Lbrace) {
            panic!("unexpected syntax of if: {}", self.cur_token.get_xml_tag());
        }

        self.advance();
        let mut if_statements = vec![];
        while !self.current_token_is_eof_or(TokenType::Rbrace) {
            if_statements.push(self.parse_statement());
            self.advance();
        }

        let else_statements = if self.peek_token_is(TokenType::Keyword(Keyword::Else)) {
            self.advance();
            self.advance();
            if !self.current_token_is(TokenType::Lbrace) {
                panic!(
                    "unexpected syntax of else: {}",
                    self.cur_token.get_xml_tag()
                );
            }

            self.advance();
            let mut statements = vec![];
            while !self.current_token_is_eof_or(TokenType::Rbrace) {
                statements.push(self.parse_statement());
                self.advance();
            }
            Some(statements)
        } else {
            None
        };

        Statement::If(expression, if_statements, else_statements)
    }

    fn parse_do_statement(&mut self) -> Statement {
        self.advance();

        let subroutine_call = if let TokenType::Identifier(identifier) = self.cur_token.clone() {
            self.parse_subroutine_call(identifier)
        } else {
            panic!(
                "unexpected syntax of do statement: {}",
                self.cur_token.get_xml_tag()
            );
        };

        while !self.current_token_is_eof_or(TokenType::Semicolon) {
            self.advance();
        }

        Statement::Do(subroutine_call)
    }

    fn parse_expression_statement(&mut self) -> Statement {
        let expression = self.parse_expression();

        while !self.current_token_is_eof_or(TokenType::Semicolon) {
            self.advance();
        }
        Statement::Expression(expression)
    }

    fn parse_expression(&mut self) -> Expression {
        let left_term = self.parse_term();

        // TODO: op precedence is not implemented
        match self.parse_binary_op() {
            Some(binary_op) => Expression::new_binary_op(left_term, binary_op),
            None => Expression::new(left_term),
        }
    }

    fn parse_subroutine_call(&mut self, identifier_token: IdentifierToken) -> SubroutineCall {
        let parent_name = if self.peek_token_is(TokenType::Dot) {
            self.advance();
            self.advance();
            Some(identifier_token)
        } else {
            None
        };

        let subroutine_name =
            if let TokenType::Identifier(identifier_token) = self.cur_token.clone() {
                identifier_token
            } else {
                panic!(
                    "unexpected syntax of subroutine call: {}",
                    self.cur_token.get_xml_tag()
                );
            };

        self.advance();
        if !self.current_token_is(TokenType::Lparen) {
            panic!(
                "unexpected syntax of subroutine call: {}",
                self.cur_token.get_xml_tag()
            );
        }

        self.advance();
        let mut expressions = vec![];
        while !self.current_token_is_eof_or(TokenType::Rparen) {
            expressions.push(self.parse_expression());

            if self.peek_token_is(TokenType::Comma) {
                self.advance();
                self.advance();
            } else if self.peek_token_is(TokenType::Rparen) {
                self.advance();
            } else {
                panic!(
                    "unexpected syntax of subroutine call: {}",
                    self.cur_token.get_xml_tag()
                );
            }
        }

        match parent_name {
            Some(parent_name) => SubroutineCall::new_parent_subroutine_call(
                parent_name,
                subroutine_name,
                expressions,
            ),
            None => SubroutineCall::new(subroutine_name, expressions),
        }
    }

    fn parse_var_name(&mut self, identifier_token: IdentifierToken) -> Term {
        let identifier_token = identifier_token;

        if self.peek_token_is(TokenType::Lbracket) {
            self.advance();
            self.advance();

            let expression = self.parse_expression();

            if !self.peek_token_is(TokenType::Rbracket) {
                panic!("unexpected syntax of varName: {:?}", self.peek_token());
            }

            self.advance();

            Term::VarName(identifier_token, Some(Box::new(expression)))
        } else {
            Term::VarName(identifier_token, None)
        }
    }

    fn parse_expression_term(&mut self) -> Term {
        self.advance();

        let mut expression = self.parse_expression();

        if !self.peek_token_is(TokenType::Rparen) {
            expression = match self.parse_binary_op() {
                Some(binary_op) => {
                    Expression::new_binary_op(Term::Expresssion(Box::new(expression)), binary_op)
                }
                None => panic!("unexpected syntax of expression term: {:?}", self.cur_token),
            };
        }

        self.advance();

        Term::Expresssion(Box::new(expression))
    }

    fn parse_unary_op_constant(&mut self, op: TokenType) -> Term {
        self.advance();

        let term = self.parse_term();

        Term::UnaryOp(UnaryOpToken::new(op), Box::new(term))
    }

    fn parse_term(&mut self) -> Term {
        let token = self.cur_token.clone();
        match token {
            TokenType::Identifier(identifier_token) => {
                if self.peek_token_is(TokenType::Lparen) || self.peek_token_is(TokenType::Dot) {
                    Term::SubroutineCall(self.parse_subroutine_call(identifier_token))
                } else {
                    self.parse_var_name(identifier_token)
                }
            }
            TokenType::Number(num) => Term::IntegerConstant(num),
            TokenType::String(str_value) => Term::StringConstant(str_value),
            TokenType::Keyword(keyword) => {
                Term::KeywordConstant(KeywordConstantToken::new(keyword))
            }
            TokenType::Lparen => self.parse_expression_term(),
            TokenType::Minus | TokenType::Tilde => self.parse_unary_op_constant(token),
            _ => panic!(
                "unexpected token is passed to get_prefix_parse_function: {:?}",
                token
            ),
        }
    }

    fn parse_binary_op(&mut self) -> Option<BinaryOp> {
        if BinaryOpToken::is_binary_op_token_type(&self.peek_token()) {
            self.advance();
            let op = BinaryOpToken::new(self.cur_token.clone());

            self.advance();

            let right_term = self.parse_term();

            Some(BinaryOp::new(op, right_term))
        } else {
            None
        }
    }

    fn peek_token_is(&mut self, token: TokenType) -> bool {
        self.peek_token() == token
    }

    fn current_token_is(&self, token: TokenType) -> bool {
        self.cur_token == token
    }

    fn current_token_is_eof_or(&self, token: TokenType) -> bool {
        self.current_token_is(token) || self.cur_token == TokenType::Eof
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
        let z = 1 * 2;
        "
        .as_bytes();
        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse_program();

        assert_eq!(actual.statements.len(), 6);
        assert_eq!(
            actual.statements,
            vec![
                Statement::Let(
                    IdentifierToken::new("x"),
                    None,
                    Expression::new(Term::IntegerConstant(5))
                ),
                Statement::Let(
                    IdentifierToken::new("y"),
                    None,
                    Expression::new(Term::VarName(IdentifierToken::new("x"), None))
                ),
                Statement::Let(
                    IdentifierToken::new("z"),
                    Some(Expression::new(Term::VarName(
                        IdentifierToken::new("i"),
                        None
                    ))),
                    Expression::new(Term::VarName(IdentifierToken::new("y"), None)),
                ),
                Statement::Let(
                    IdentifierToken::new("z"),
                    Some(Expression::new(Term::IntegerConstant(0))),
                    Expression::new(Term::VarName(IdentifierToken::new("y"), None))
                ),
                Statement::Let(
                    IdentifierToken::new("z"),
                    None,
                    Expression::new(Term::StringConstant("foo".to_string()))
                ),
                Statement::Let(
                    IdentifierToken::new("z"),
                    None,
                    Expression::new_binary_op(
                        Term::IntegerConstant(1),
                        BinaryOp::new(
                            BinaryOpToken::new(TokenType::Asterisk),
                            Term::IntegerConstant(2)
                        )
                    )
                ),
            ]
        );
    }

    #[test]
    fn subroutine_call_statements() {
        let source = "
        hoge();
        fuga(1, 2 + 3);
        parent.hoge();
        parent.hoge(1, 2);
        "
        .as_bytes();
        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse_program();

        assert_eq!(actual.statements.len(), 4);
        assert_eq!(
            actual.statements,
            vec![
                Statement::Expression(Expression::new(Term::SubroutineCall(SubroutineCall::new(
                    IdentifierToken::new("hoge"),
                    vec![]
                )))),
                Statement::Expression(Expression::new(Term::SubroutineCall(SubroutineCall::new(
                    IdentifierToken::new("fuga"),
                    vec![
                        Expression::new(Term::IntegerConstant(1)),
                        Expression::new_binary_op(
                            Term::IntegerConstant(2),
                            BinaryOp::new(
                                BinaryOpToken::new(TokenType::Plus),
                                Term::IntegerConstant(3)
                            )
                        )
                    ]
                )))),
                Statement::Expression(Expression::new(Term::SubroutineCall(
                    SubroutineCall::new_parent_subroutine_call(
                        IdentifierToken::new("parent"),
                        IdentifierToken::new("hoge"),
                        vec![]
                    )
                ))),
                Statement::Expression(Expression::new(Term::SubroutineCall(
                    SubroutineCall::new_parent_subroutine_call(
                        IdentifierToken::new("parent"),
                        IdentifierToken::new("hoge"),
                        vec![
                            Expression::new(Term::IntegerConstant(1)),
                            Expression::new(Term::IntegerConstant(2)),
                        ]
                    )
                )))
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
                Statement::Return(Some(Expression::new(Term::IntegerConstant(5)))),
                Statement::Return(Some(Expression::new(Term::VarName(
                    IdentifierToken::new("x"),
                    None
                )))),
                Statement::Return(None)
            ]
        );
    }

    #[test]
    fn do_statements() {
        let source = "
        do game.run();
        "
        .as_bytes();
        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse_program();

        assert_eq!(actual.statements.len(), 1);
        assert_eq!(
            actual.statements,
            vec![Statement::Do(SubroutineCall::new_parent_subroutine_call(
                IdentifierToken::new("game"),
                IdentifierToken::new("run"),
                vec![],
            ),)]
        );
    }

    #[test]
    fn while_statements() {
        let source = "
        while (true) {
            let i = 1;
            i;
        }

        while ((2 * 3)) {
            i;
        }
        "
        .as_bytes();
        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse_program();

        assert_eq!(actual.statements.len(), 2);
        assert_eq!(
            actual.statements,
            vec![
                Statement::While(
                    Expression::new(Term::KeywordConstant(KeywordConstantToken::new(
                        Keyword::True
                    ))),
                    vec![
                        Statement::Let(
                            IdentifierToken::new("i"),
                            None,
                            Expression::new(Term::IntegerConstant(1)),
                        ),
                        Statement::Expression(Expression::new(Term::VarName(
                            IdentifierToken::new("i"),
                            None
                        )))
                    ]
                ),
                Statement::While(
                    Expression::new(Term::Expresssion(Box::new(Expression::new_binary_op(
                        Term::IntegerConstant(2),
                        BinaryOp::new(
                            BinaryOpToken::new(TokenType::Asterisk),
                            Term::IntegerConstant(3)
                        )
                    )))),
                    vec![Statement::Expression(Expression::new(Term::VarName(
                        IdentifierToken::new("i"),
                        None
                    )))]
                )
            ]
        );
    }

    #[test]
    fn if_statements() {
        let source = "
        if (true) {
            let i = 1;
            i;
        }

        if ((2 * 3)) {
            i;
        } else {
            let j= 2;
            j;
        }
        "
        .as_bytes();
        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse_program();

        assert_eq!(actual.statements.len(), 2);
        assert_eq!(
            actual.statements,
            vec![
                Statement::If(
                    Expression::new(Term::KeywordConstant(KeywordConstantToken::new(
                        Keyword::True
                    ))),
                    vec![
                        Statement::Let(
                            IdentifierToken::new("i"),
                            None,
                            Expression::new(Term::IntegerConstant(1)),
                        ),
                        Statement::Expression(Expression::new(Term::VarName(
                            IdentifierToken::new("i"),
                            None
                        )))
                    ],
                    None
                ),
                Statement::If(
                    Expression::new(Term::Expresssion(Box::new(Expression::new_binary_op(
                        Term::IntegerConstant(2),
                        BinaryOp::new(
                            BinaryOpToken::new(TokenType::Asterisk),
                            Term::IntegerConstant(3)
                        )
                    )))),
                    vec![Statement::Expression(Expression::new(Term::VarName(
                        IdentifierToken::new("i"),
                        None
                    )))],
                    Some(vec![
                        Statement::Let(
                            IdentifierToken::new("j"),
                            None,
                            Expression::new(Term::IntegerConstant(2)),
                        ),
                        Statement::Expression(Expression::new(Term::VarName(
                            IdentifierToken::new("j"),
                            None
                        )))
                    ])
                )
            ]
        );
    }

    #[test]
    fn var_name_expression() {
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
            vec![Statement::Expression(Expression::new(Term::VarName(
                IdentifierToken::new("foobar"),
                None
            )))]
        );
    }

    #[test]
    fn var_name_has_index_expression() {
        let source = "
        foobar[1];
        foobar[  2 ];
        foobar[i + 1];
        "
        .as_bytes();
        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse_program();

        assert_eq!(actual.statements.len(), 3);
        assert_eq!(
            actual.statements,
            vec![
                Statement::Expression(Expression::new(Term::VarName(
                    IdentifierToken::new("foobar"),
                    Some(Box::new(Expression::new(Term::IntegerConstant(1))))
                ))),
                Statement::Expression(Expression::new(Term::VarName(
                    IdentifierToken::new("foobar"),
                    Some(Box::new(Expression::new(Term::IntegerConstant(2))))
                ))),
                Statement::Expression(Expression::new(Term::VarName(
                    IdentifierToken::new("foobar"),
                    Some(Box::new(Expression::new_binary_op(
                        Term::VarName(IdentifierToken::new("i"), None),
                        BinaryOp::new(
                            BinaryOpToken::new(TokenType::Plus),
                            Term::IntegerConstant(1)
                        )
                    )))
                )))
            ]
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
            vec![Statement::Expression(Expression::new(
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
            vec![Statement::Expression(Expression::new(
                Term::StringConstant("str value!!".to_string())
            ))]
        );
    }

    #[test]
    fn keyword_constant_expression() {
        let source = "
        this;
        null;
        false;
        "
        .as_bytes();
        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse_program();

        assert_eq!(actual.statements.len(), 3);
        assert_eq!(
            actual.statements,
            vec![
                Statement::Expression(Expression::new(Term::KeywordConstant(
                    KeywordConstantToken::new(Keyword::This)
                ))),
                Statement::Expression(Expression::new(Term::KeywordConstant(
                    KeywordConstantToken::new(Keyword::Null)
                ))),
                Statement::Expression(Expression::new(Term::KeywordConstant(
                    KeywordConstantToken::new(Keyword::False)
                )))
            ]
        );
    }

    #[test]
    fn expression_term() {
        let source = "
        (-1);
        -1 + (2 * 3);
        -4 + ((5 - 6) / 7);
        -8 +( ((9 - 10) / 11) * 12);
        "
        .as_bytes();
        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse_program();

        assert_eq!(actual.statements.len(), 4);
        assert_eq!(
            actual.statements,
            vec![
                Statement::Expression(Expression::new(Term::Expresssion(Box::new(
                    Expression::new(Term::UnaryOp(
                        UnaryOpToken::new(TokenType::Minus),
                        Box::new(Term::IntegerConstant(1))
                    ))
                )))),
                Statement::Expression(Expression::new_binary_op(
                    Term::UnaryOp(
                        UnaryOpToken::new(TokenType::Minus),
                        Box::new(Term::IntegerConstant(1))
                    ),
                    BinaryOp::new(
                        BinaryOpToken::new(TokenType::Plus),
                        Term::Expresssion(Box::new(Expression::new_binary_op(
                            Term::IntegerConstant(2),
                            BinaryOp::new(
                                BinaryOpToken::new(TokenType::Asterisk),
                                Term::IntegerConstant(3)
                            )
                        )))
                    )
                )),
                // -4 + ((5 - 6) / 7);
                Statement::Expression(Expression::new_binary_op(
                    Term::UnaryOp(
                        UnaryOpToken::new(TokenType::Minus),
                        Box::new(Term::IntegerConstant(4))
                    ),
                    BinaryOp::new(
                        BinaryOpToken::new(TokenType::Plus),
                        Term::Expresssion(Box::new(Expression::new_binary_op(
                            Term::Expresssion(Box::new(Expression::new_binary_op(
                                Term::IntegerConstant(5),
                                BinaryOp::new(
                                    BinaryOpToken::new(TokenType::Minus),
                                    Term::IntegerConstant(6)
                                )
                            ))),
                            BinaryOp::new(
                                BinaryOpToken::new(TokenType::Slash),
                                Term::IntegerConstant(7)
                            )
                        )))
                    )
                )),
                // -8 + (((9 - 10) / 11) * 12);
                Statement::Expression(Expression::new_binary_op(
                    Term::UnaryOp(
                        UnaryOpToken::new(TokenType::Minus),
                        Box::new(Term::IntegerConstant(8))
                    ),
                    BinaryOp::new(
                        BinaryOpToken::new(TokenType::Plus),
                        Term::Expresssion(Box::new(Expression::new_binary_op(
                            Term::Expresssion(Box::new(Expression::new_binary_op(
                                Term::Expresssion(Box::new(Expression::new_binary_op(
                                    Term::IntegerConstant(9),
                                    BinaryOp::new(
                                        BinaryOpToken::new(TokenType::Minus),
                                        Term::IntegerConstant(10)
                                    )
                                ))),
                                BinaryOp::new(
                                    BinaryOpToken::new(TokenType::Slash),
                                    Term::IntegerConstant(11)
                                )
                            ))),
                            BinaryOp::new(
                                BinaryOpToken::new(TokenType::Asterisk),
                                Term::IntegerConstant(12)
                            )
                        )))
                    )
                )),
            ]
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
                Statement::Expression(Expression::new(Term::UnaryOp(
                    UnaryOpToken::new(TokenType::Minus),
                    Box::new(Term::IntegerConstant(1))
                ))),
                Statement::Expression(Expression::new(Term::UnaryOp(
                    UnaryOpToken::new(TokenType::Tilde),
                    Box::new(Term::VarName(IdentifierToken::new("i",), None))
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
                Statement::Expression(Expression::new_binary_op(
                    Term::IntegerConstant(1),
                    BinaryOp::new(
                        BinaryOpToken::new(TokenType::Plus),
                        Term::VarName(IdentifierToken::new("i"), None)
                    )
                )),
                Statement::Expression(Expression::new_binary_op(
                    Term::IntegerConstant(2),
                    BinaryOp::new(BinaryOpToken::new(TokenType::Gt), Term::IntegerConstant(3),)
                ))
            ]
        );
    }
}
