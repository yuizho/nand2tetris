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
            TokenType::KEYWORD(Keyword::LET) => self.parse_let_statement(),
            TokenType::KEYWORD(Keyword::RETURN) => self.parse_return_statement(),
            TokenType::KEYWORD(Keyword::WHILE) => self.parse_while_statement(),
            TokenType::KEYWORD(Keyword::IF) => self.parse_if_statement(),
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

    fn parse_while_statement(&mut self) -> Statement {
        self.advance();
        if !self.current_token_is(TokenType::LPAREN) {
            panic!(
                "unexpected syntax of while: {}",
                self.cur_token.get_xml_tag()
            );
        }

        self.advance();
        let expression = self.parse_expression();

        self.advance();
        if !self.current_token_is(TokenType::RPAREN) {
            panic!(
                "unexpected syntax of while: {}",
                self.cur_token.get_xml_tag()
            );
        }

        self.advance();
        if !self.current_token_is(TokenType::LBRACE) {
            panic!(
                "unexpected syntax of while: {}",
                self.cur_token.get_xml_tag()
            );
        }

        self.advance();
        let mut statements = vec![];
        while !self.current_token_is_eof_or(TokenType::RBRACE) {
            statements.push(self.parse_statement());
            self.advance();
        }

        Statement::WhileStatement(expression, statements)
    }

    fn parse_if_statement(&mut self) -> Statement {
        self.advance();
        if !self.current_token_is(TokenType::LPAREN) {
            panic!("unexpected syntax of if: {}", self.cur_token.get_xml_tag());
        }

        self.advance();
        let expression = self.parse_expression();

        self.advance();
        if !self.current_token_is(TokenType::RPAREN) {
            panic!("unexpected syntax of if: {}", self.cur_token.get_xml_tag());
        }

        self.advance();
        if !self.current_token_is(TokenType::LBRACE) {
            panic!("unexpected syntax of if: {}", self.cur_token.get_xml_tag());
        }

        self.advance();
        let mut if_statements = vec![];
        while !self.current_token_is_eof_or(TokenType::RBRACE) {
            if_statements.push(self.parse_statement());
            self.advance();
        }

        let else_statements = if self.peek_token_is(TokenType::KEYWORD(Keyword::ELSE)) {
            self.advance();
            self.advance();
            if !self.current_token_is(TokenType::LBRACE) {
                panic!(
                    "unexpected syntax of else: {}",
                    self.cur_token.get_xml_tag()
                );
            }

            self.advance();
            let mut statements = vec![];
            while !self.current_token_is_eof_or(TokenType::RBRACE) {
                statements.push(self.parse_statement());
                self.advance();
            }
            Some(statements)
        } else {
            None
        };

        Statement::IfStatement(expression, if_statements, else_statements)
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
        match self.parse_binary_op() {
            Some(binary_op) => Expression::new_binary_op(left_term, binary_op),
            None => Expression::new(left_term),
        }
    }

    fn parse_subroutine_call(&mut self, identifier_token: IdentifierToken) -> Term {
        let parent_name = if self.peek_token_is(TokenType::DOT) {
            self.advance();
            self.advance();
            Some(identifier_token)
        } else {
            None
        };

        let subroutine_name =
            if let TokenType::IDNETIFIER(identifier_token) = self.cur_token.clone() {
                identifier_token
            } else {
                panic!(
                    "unexpected syntax of subroutine call: {}",
                    self.cur_token.get_xml_tag()
                );
            };

        self.advance();
        if !self.current_token_is(TokenType::LPAREN) {
            panic!(
                "unexpected syntax of subroutine call: {}",
                self.cur_token.get_xml_tag()
            );
        }

        self.advance();
        let mut expressions = vec![];
        while !self.current_token_is_eof_or(TokenType::RPAREN) {
            expressions.push(self.parse_expression());

            if self.peek_token_is(TokenType::COMMA) {
                self.advance();
                self.advance();
            } else if self.peek_token_is(TokenType::RPAREN) {
                self.advance();
            } else {
                panic!(
                    "unexpected syntax of subroutine call: {}",
                    self.cur_token.get_xml_tag()
                );
            }
        }

        let subroutine_call = match parent_name {
            Some(parent_name) => SubroutineCall::new_parent_subroutine_call(
                parent_name,
                subroutine_name,
                expressions,
            ),
            None => SubroutineCall::new(subroutine_name, expressions),
        };
        Term::SubroutineCall(subroutine_call)
    }

    fn parse_var_name(&mut self, identifier_token: IdentifierToken) -> Term {
        let identifier_token = identifier_token;

        if self.peek_token_is(TokenType::LBRACKET) {
            self.advance();
            self.advance();

            let expression = self.parse_expression();

            if !self.peek_token_is(TokenType::RBRACKET) {
                panic!("unexpected syntax of varName: {:?}", self.peek_token);
            }

            self.advance();

            Term::VarName(identifier_token, Some(Box::new(expression)))
        } else {
            Term::VarName(identifier_token, None)
        }
    }

    fn parse_integer_constant(&self, num: i32) -> Term {
        Term::IntegerConstant(num)
    }

    fn parse_string_constant(&self, str_value: String) -> Term {
        Term::StringConstant(str_value)
    }

    fn parse_keyword_constant(&self, keyword: KeywordConstantToken) -> Term {
        Term::KeywordConstant(keyword)
    }

    fn parse_expression_term(&mut self) -> Term {
        self.advance();

        let mut expression = self.parse_expression();

        if !self.peek_token_is(TokenType::RPAREN) {
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
            TokenType::IDNETIFIER(identifier_token) => {
                if self.peek_token_is(TokenType::LPAREN) || self.peek_token_is(TokenType::DOT) {
                    self.parse_subroutine_call(identifier_token)
                } else {
                    self.parse_var_name(identifier_token)
                }
            }
            TokenType::NUMBER(num) => self.parse_integer_constant(num),
            TokenType::STRING(str_value) => self.parse_string_constant(str_value),
            TokenType::KEYWORD(keyword) => {
                self.parse_keyword_constant(KeywordConstantToken::new(keyword))
            }
            TokenType::LPAREN => self.parse_expression_term(),
            TokenType::MINUS | TokenType::TILDE => self.parse_unary_op_constant(token),
            _ => panic!(
                "unexpected token is passed to get_prefix_parse_function: {:?}",
                token
            ),
        }
    }

    fn parse_binary_op(&mut self) -> Option<BinaryOp> {
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

    fn peek_token_is(&self, token: TokenType) -> bool {
        self.peek_token == token
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
                Statement::LetStatement(
                    IdentifierToken::new("x"),
                    None,
                    Expression::new(Term::IntegerConstant(5))
                ),
                Statement::LetStatement(
                    IdentifierToken::new("y"),
                    None,
                    Expression::new(Term::VarName(IdentifierToken::new("x"), None))
                ),
                Statement::LetStatement(
                    IdentifierToken::new("z"),
                    Some(Expression::new(Term::VarName(
                        IdentifierToken::new("i"),
                        None
                    ))),
                    Expression::new(Term::VarName(IdentifierToken::new("y"), None)),
                ),
                Statement::LetStatement(
                    IdentifierToken::new("z"),
                    Some(Expression::new(Term::IntegerConstant(0))),
                    Expression::new(Term::VarName(IdentifierToken::new("y"), None))
                ),
                Statement::LetStatement(
                    IdentifierToken::new("z"),
                    None,
                    Expression::new(Term::StringConstant("foo".to_string()))
                ),
                Statement::LetStatement(
                    IdentifierToken::new("z"),
                    None,
                    Expression::new_binary_op(
                        Term::IntegerConstant(1),
                        BinaryOp::new(
                            BinaryOpToken::new(TokenType::ASTERISK),
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
                Statement::ExpressionStatement(Expression::new(Term::SubroutineCall(
                    SubroutineCall::new(IdentifierToken::new("hoge"), vec![])
                ))),
                Statement::ExpressionStatement(Expression::new(Term::SubroutineCall(
                    SubroutineCall::new(
                        IdentifierToken::new("fuga"),
                        vec![
                            Expression::new(Term::IntegerConstant(1)),
                            Expression::new_binary_op(
                                Term::IntegerConstant(2),
                                BinaryOp::new(
                                    BinaryOpToken::new(TokenType::PLUS),
                                    Term::IntegerConstant(3)
                                )
                            )
                        ]
                    )
                ))),
                Statement::ExpressionStatement(Expression::new(Term::SubroutineCall(
                    SubroutineCall::new_parent_subroutine_call(
                        IdentifierToken::new("parent"),
                        IdentifierToken::new("hoge"),
                        vec![]
                    )
                ))),
                Statement::ExpressionStatement(Expression::new(Term::SubroutineCall(
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
                Statement::ReturnStatement(Some(Expression::new(Term::IntegerConstant(5)))),
                Statement::ReturnStatement(Some(Expression::new(Term::VarName(
                    IdentifierToken::new("x"),
                    None
                )))),
                Statement::ReturnStatement(None)
            ]
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
                Statement::WhileStatement(
                    Expression::new(Term::KeywordConstant(KeywordConstantToken::new(
                        Keyword::TRUE
                    ))),
                    vec![
                        Statement::LetStatement(
                            IdentifierToken::new("i"),
                            None,
                            Expression::new(Term::IntegerConstant(1)),
                        ),
                        Statement::ExpressionStatement(Expression::new(Term::VarName(
                            IdentifierToken::new("i"),
                            None
                        )))
                    ]
                ),
                Statement::WhileStatement(
                    Expression::new(Term::Expresssion(Box::new(Expression::new_binary_op(
                        Term::IntegerConstant(2),
                        BinaryOp::new(
                            BinaryOpToken::new(TokenType::ASTERISK),
                            Term::IntegerConstant(3)
                        )
                    )))),
                    vec![Statement::ExpressionStatement(Expression::new(
                        Term::VarName(IdentifierToken::new("i"), None)
                    ))]
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
                Statement::IfStatement(
                    Expression::new(Term::KeywordConstant(KeywordConstantToken::new(
                        Keyword::TRUE
                    ))),
                    vec![
                        Statement::LetStatement(
                            IdentifierToken::new("i"),
                            None,
                            Expression::new(Term::IntegerConstant(1)),
                        ),
                        Statement::ExpressionStatement(Expression::new(Term::VarName(
                            IdentifierToken::new("i"),
                            None
                        )))
                    ],
                    None
                ),
                Statement::IfStatement(
                    Expression::new(Term::Expresssion(Box::new(Expression::new_binary_op(
                        Term::IntegerConstant(2),
                        BinaryOp::new(
                            BinaryOpToken::new(TokenType::ASTERISK),
                            Term::IntegerConstant(3)
                        )
                    )))),
                    vec![Statement::ExpressionStatement(Expression::new(
                        Term::VarName(IdentifierToken::new("i"), None)
                    ))],
                    Some(vec![
                        Statement::LetStatement(
                            IdentifierToken::new("j"),
                            None,
                            Expression::new(Term::IntegerConstant(2)),
                        ),
                        Statement::ExpressionStatement(Expression::new(Term::VarName(
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
            vec![Statement::ExpressionStatement(Expression::new(
                Term::VarName(IdentifierToken::new("foobar"), None)
            ))]
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
                Statement::ExpressionStatement(Expression::new(Term::VarName(
                    IdentifierToken::new("foobar"),
                    Some(Box::new(Expression::new(Term::IntegerConstant(1))))
                ))),
                Statement::ExpressionStatement(Expression::new(Term::VarName(
                    IdentifierToken::new("foobar"),
                    Some(Box::new(Expression::new(Term::IntegerConstant(2))))
                ))),
                Statement::ExpressionStatement(Expression::new(Term::VarName(
                    IdentifierToken::new("foobar"),
                    Some(Box::new(Expression::new_binary_op(
                        Term::VarName(IdentifierToken::new("i"), None),
                        BinaryOp::new(
                            BinaryOpToken::new(TokenType::PLUS),
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
                Statement::ExpressionStatement(Expression::new(Term::KeywordConstant(
                    KeywordConstantToken::new(Keyword::THIS)
                ))),
                Statement::ExpressionStatement(Expression::new(Term::KeywordConstant(
                    KeywordConstantToken::new(Keyword::NULL)
                ))),
                Statement::ExpressionStatement(Expression::new(Term::KeywordConstant(
                    KeywordConstantToken::new(Keyword::FALSE)
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
                Statement::ExpressionStatement(Expression::new(Term::Expresssion(Box::new(
                    Expression::new(Term::UnaryOp(
                        UnaryOpToken::new(TokenType::MINUS),
                        Box::new(Term::IntegerConstant(1))
                    ))
                )))),
                Statement::ExpressionStatement(Expression::new_binary_op(
                    Term::UnaryOp(
                        UnaryOpToken::new(TokenType::MINUS),
                        Box::new(Term::IntegerConstant(1))
                    ),
                    BinaryOp::new(
                        BinaryOpToken::new(TokenType::PLUS),
                        Term::Expresssion(Box::new(Expression::new_binary_op(
                            Term::IntegerConstant(2),
                            BinaryOp::new(
                                BinaryOpToken::new(TokenType::ASTERISK),
                                Term::IntegerConstant(3)
                            )
                        )))
                    )
                )),
                // -4 + ((5 - 6) / 7);
                Statement::ExpressionStatement(Expression::new_binary_op(
                    Term::UnaryOp(
                        UnaryOpToken::new(TokenType::MINUS),
                        Box::new(Term::IntegerConstant(4))
                    ),
                    BinaryOp::new(
                        BinaryOpToken::new(TokenType::PLUS),
                        Term::Expresssion(Box::new(Expression::new_binary_op(
                            Term::Expresssion(Box::new(Expression::new_binary_op(
                                Term::IntegerConstant(5),
                                BinaryOp::new(
                                    BinaryOpToken::new(TokenType::MINUS),
                                    Term::IntegerConstant(6)
                                )
                            ))),
                            BinaryOp::new(
                                BinaryOpToken::new(TokenType::SLASH),
                                Term::IntegerConstant(7)
                            )
                        )))
                    )
                )),
                // -8 + (((9 - 10) / 11) * 12);
                Statement::ExpressionStatement(Expression::new_binary_op(
                    Term::UnaryOp(
                        UnaryOpToken::new(TokenType::MINUS),
                        Box::new(Term::IntegerConstant(8))
                    ),
                    BinaryOp::new(
                        BinaryOpToken::new(TokenType::PLUS),
                        Term::Expresssion(Box::new(Expression::new_binary_op(
                            Term::Expresssion(Box::new(Expression::new_binary_op(
                                Term::Expresssion(Box::new(Expression::new_binary_op(
                                    Term::IntegerConstant(9),
                                    BinaryOp::new(
                                        BinaryOpToken::new(TokenType::MINUS),
                                        Term::IntegerConstant(10)
                                    )
                                ))),
                                BinaryOp::new(
                                    BinaryOpToken::new(TokenType::SLASH),
                                    Term::IntegerConstant(11)
                                )
                            ))),
                            BinaryOp::new(
                                BinaryOpToken::new(TokenType::ASTERISK),
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
                Statement::ExpressionStatement(Expression::new(Term::UnaryOp(
                    UnaryOpToken::new(TokenType::MINUS),
                    Box::new(Term::IntegerConstant(1))
                ))),
                Statement::ExpressionStatement(Expression::new(Term::UnaryOp(
                    UnaryOpToken::new(TokenType::TILDE),
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
                Statement::ExpressionStatement(Expression::new_binary_op(
                    Term::IntegerConstant(1),
                    BinaryOp::new(
                        BinaryOpToken::new(TokenType::PLUS),
                        Term::VarName(IdentifierToken::new("i"), None)
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
