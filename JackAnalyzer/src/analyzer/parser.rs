use super::ast::*;
use super::token::*;
use super::tokenizer::*;
use anyhow::{anyhow, Result};
use uuid::Uuid;

pub trait LabelGenerator {
    fn generate(&self) -> String;
}

struct UuidLabelGenerator;
impl UuidLabelGenerator {
    fn new() -> Self {
        Self
    }
}
impl LabelGenerator for UuidLabelGenerator {
    fn generate(&self) -> String {
        Uuid::new_v4().to_string()
    }
}

pub struct Parser<'a> {
    tokenizer: &'a mut JackTokenizer,
    label_generator: Box<dyn LabelGenerator>,
}
impl<'a> Parser<'a> {
    pub fn new(tokenizer: &'a mut JackTokenizer) -> Self {
        Parser {
            tokenizer,
            label_generator: Box::new(UuidLabelGenerator::new()),
        }
    }

    pub fn new_by_label_generator(
        tokenizer: &'a mut JackTokenizer,
        label_generator: Box<dyn LabelGenerator>,
    ) -> Self {
        Parser {
            tokenizer,
            label_generator: label_generator,
        }
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let token = self.next_token()?;
        if !token.is(&TokenType::Keyword(Keyword::Class)) {
            return Err(anyhow!("the code doesn't start class keyword: {:?}", token));
        }

        let token = self.next_token()?;
        let class_name = if let TokenType::Identifier(identifier) = token {
            identifier
        } else {
            return Err(anyhow!("unexpected syntax of class: {:?}", token));
        };

        let token = self.next_token()?;
        if !token.is(&TokenType::Lbrace) {
            return Err(anyhow!("unexpected syntax of class: {:?}", token));
        }

        let mut token = self.next_token()?;
        let mut var_dec = vec![];
        let mut subroutine_dec = vec![];
        while !token.is_eof_or(&TokenType::Rbrace) {
            match &token {
                TokenType::Keyword(Keyword::Static) | TokenType::Keyword(Keyword::Field) => {
                    var_dec.push(self.parse_class_var(token)?);
                }
                TokenType::Keyword(Keyword::Constructor)
                | TokenType::Keyword(Keyword::Function)
                | TokenType::Keyword(Keyword::Method) => {
                    subroutine_dec.push(self.parse_subroutine(class_name.clone(), token)?)
                }
                _ => return Err(anyhow!("unexpected syntax of class: {:?}", token)),
            }
            token = self.next_token()?;
        }

        Ok(Program::new(class_name, var_dec, subroutine_dec))
    }

    fn next_token(&mut self) -> Result<TokenType> {
        self.tokenizer.advance()
    }

    fn peek_token(&mut self) -> Result<TokenType> {
        self.tokenizer.peek()
    }

    fn parse_class_var(&mut self, var_identifier: TokenType) -> Result<ClassVarDec> {
        let token = self.next_token()?;
        let class_type_token = ClassTypeToken::new(token);

        let token = self.next_token()?;
        let var_name = if let TokenType::Identifier(identifier) = token {
            identifier
        } else {
            return Err(anyhow!("unexpected syntax of class var: {:?}", token));
        };

        let mut token = self.next_token()?;
        let mut alt_var_names = vec![];
        while !token.is_eof_or(&TokenType::Semicolon) {
            if !token.is(&TokenType::Comma) {
                return Err(anyhow!("unexpected syntax of class var: {:?}", token));
            }

            let alt_var_name = self.next_token()?;
            if let TokenType::Identifier(identifier) = alt_var_name {
                alt_var_names.push(identifier);
            } else {
                return Err(anyhow!(
                    "unexpected syntax of class var: {:?}",
                    alt_var_name
                ));
            };

            token = self.next_token()?;
        }

        Ok(ClassVarDec::new(
            var_identifier,
            class_type_token,
            var_name,
            alt_var_names,
        ))
    }

    fn parse_subroutine(
        &mut self,
        class_name: IdentifierToken,
        subroutine_identifier: TokenType,
    ) -> Result<SubroutineDec> {
        // parse return type
        let token = self.next_token()?;
        let return_type = if let TokenType::Keyword(Keyword::Void) = token {
            None
        } else {
            Some(ClassTypeToken::new(token))
        };

        // parse subroutine name
        let token = self.next_token()?;
        let subroutine_name = if let TokenType::Identifier(identifier) = token {
            identifier
        } else {
            return Err(anyhow!("unexpected syntax of subroutine: {:?}", token));
        };

        // parse parameters
        let token = self.next_token()?;
        if !token.is(&TokenType::Lparen) {
            return Err(anyhow!("unexpected syntax of subroutine: {:?}", token));
        }

        let mut token = self.next_token()?;
        let mut parameters = vec![];
        while !token.is_eof_or(&TokenType::Rparen) {
            if token.is(&TokenType::Comma) {
                token = self.next_token()?;
            }
            let var_type = ClassTypeToken::new(token);
            let var_name = self.next_token()?;
            let param_name = if let TokenType::Identifier(identifier) = var_name {
                identifier
            } else {
                return Err(anyhow!("unexpected syntax of subroutine: {:?}", var_name));
            };
            parameters.push((var_type, param_name));

            token = self.next_token()?;
        }

        let token = self.next_token()?;
        if !token.is(&TokenType::Lbrace) {
            return Err(anyhow!("unexpected syntax of subroutine: {:?}", token));
        }

        // parse subroutine body
        let mut token = self.next_token()?;
        let mut var_dec = vec![];
        let mut statements = vec![];
        while !token.is_eof_or(&TokenType::Rbrace) {
            match &token {
                TokenType::Keyword(Keyword::Var) => var_dec.push(self.parse_local_var()?),
                _ => statements.push(self.parse_statement(token)?),
            }
            token = self.next_token()?;
        }

        Ok(SubroutineDec::new(
            class_name,
            subroutine_identifier,
            return_type,
            subroutine_name,
            parameters,
            var_dec,
            statements,
        ))
    }

    fn parse_local_var(&mut self) -> Result<VarDec> {
        let token = self.next_token()?;
        let var_type = ClassTypeToken::new(token);

        let token = self.next_token()?;
        let var_name = if let TokenType::Identifier(identifier) = token {
            identifier
        } else {
            return Err(anyhow!("unexpected syntax of class var: {:?}", token));
        };

        let mut token = self.next_token()?;
        let mut alt_var_names = vec![];
        while !token.is_eof_or(&TokenType::Semicolon) {
            if !token.is(&TokenType::Comma) {
                return Err(anyhow!("unexpected syntax of class var: {:?}", token));
            }

            let alt_var_name = self.next_token()?;
            if let TokenType::Identifier(identifier) = alt_var_name {
                alt_var_names.push(identifier);
            } else {
                return Err(anyhow!(
                    "unexpected syntax of class var: {:?}",
                    alt_var_name
                ));
            };

            token = self.next_token()?;
        }

        Ok(VarDec::new(var_type, var_name, alt_var_names))
    }

    fn parse_statement(&mut self, token: TokenType) -> Result<Statement> {
        match &token {
            TokenType::Keyword(Keyword::Let) => self.parse_let_statement(),
            TokenType::Keyword(Keyword::Return) => self.parse_return_statement(),
            TokenType::Keyword(Keyword::While) => self.parse_while_statement(),
            TokenType::Keyword(Keyword::If) => self.parse_if_statement(),
            TokenType::Keyword(Keyword::Do) => self.parse_do_statement(),
            _ => self.parse_expression_statement(token),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
        let token = self.next_token()?;

        let identifier_token = if let TokenType::Identifier(identifier_token) = token {
            identifier_token
        } else {
            return Err(anyhow!(
                "failed to get identifier keyword of let: {:?}",
                token
            ));
        };

        let token = self.next_token()?;
        let (index_expression, token) = if token.is(&TokenType::Lbracket) {
            let token = self.next_token()?;
            let expression = self.parse_expression(token)?;

            let token = self.next_token()?;
            if !token.is(&TokenType::Rbracket) {
                return Err(anyhow!("unexpected syntax of let: {:?}", token));
            }

            (Some(expression), self.next_token()?)
        } else {
            (None, token)
        };

        if !token.is(&TokenType::Assign) {
            return Err(anyhow!("unexpected syntax of let: {:?}", token));
        }

        let token = self.next_token()?;
        let expression = self.parse_expression(token)?;

        let mut token = self.next_token()?;
        while !token.is_eof_or(&TokenType::Semicolon) {
            token = self.next_token()?;
        }

        Ok(Statement::Let(
            identifier_token,
            index_expression,
            expression,
        ))
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        let token = self.next_token()?;
        let expression = if token.is(&TokenType::Semicolon) {
            None
        } else {
            let expression = Some(self.parse_expression(token)?);
            let token = self.next_token()?;
            if !token.is(&TokenType::Semicolon) {
                return Err(anyhow!("unexpected syntax of return: {:?}", token));
            }
            expression
        };

        Ok(Statement::Return(expression))
    }

    fn parse_while_statement(&mut self) -> Result<Statement> {
        let token = self.next_token()?;
        if !token.is(&TokenType::Lparen) {
            return Err(anyhow!("unexpected syntax of while: {:?}", token));
        }

        let token = self.next_token()?;
        let expression = self.parse_expression(token)?;

        let token = self.next_token()?;
        if !token.is(&TokenType::Rparen) {
            return Err(anyhow!("unexpected syntax of while: {:?}", token));
        }

        let token = self.next_token()?;
        if !token.is(&TokenType::Lbrace) {
            return Err(anyhow!("unexpected syntax of while: {:?}", token));
        }

        let mut token = self.next_token()?;
        let mut statements = vec![];
        while !token.is_eof_or(&TokenType::Rbrace) {
            statements.push(self.parse_statement(token)?);
            token = self.next_token()?;
        }

        Ok(Statement::While(
            expression,
            self.label_generator.generate(),
            statements,
        ))
    }

    fn parse_if_statement(&mut self) -> Result<Statement> {
        let token = self.next_token()?;
        if !token.is(&TokenType::Lparen) {
            return Err(anyhow!("unexpected syntax of if: {:?}", token));
        }

        let token = self.next_token()?;
        let expression = self.parse_expression(token)?;

        let token = self.next_token()?;
        if !token.is(&TokenType::Rparen) {
            return Err(anyhow!("unexpected syntax of if: {:?}", token));
        }

        let token = self.next_token()?;
        if !token.is(&TokenType::Lbrace) {
            return Err(anyhow!("unexpected syntax of if: {:?}", token));
        }

        let mut token = self.next_token()?;
        let mut if_statements = vec![];
        while !token.is_eof_or(&TokenType::Rbrace) {
            if_statements.push(self.parse_statement(token)?);
            token = self.next_token()?;
        }

        let else_statements = if self.peek_token_is(TokenType::Keyword(Keyword::Else))? {
            self.next_token()?;
            let token = self.next_token()?;
            if !token.is(&TokenType::Lbrace) {
                return Err(anyhow!("unexpected syntax of else: {:?}", token));
            }

            let mut token = self.next_token()?;
            let mut statements = vec![];
            while !token.is_eof_or(&TokenType::Rbrace) {
                statements.push(self.parse_statement(token)?);
                token = self.next_token()?;
            }
            Some(statements)
        } else {
            None
        };

        Ok(Statement::If(
            expression,
            self.label_generator.generate(),
            if_statements,
            else_statements,
        ))
    }

    fn parse_do_statement(&mut self) -> Result<Statement> {
        let token = self.next_token()?;
        let subroutine_call = if let TokenType::Identifier(identifier) = token {
            self.parse_subroutine_call(identifier)?
        } else {
            return Err(anyhow!("unexpected syntax of do statement: {:?}", token));
        };

        let mut token = self.next_token()?;
        while !token.is_eof_or(&TokenType::Semicolon) {
            token = self.next_token()?;
        }

        Ok(Statement::Do(subroutine_call))
    }

    fn parse_expression_statement(&mut self, token: TokenType) -> Result<Statement> {
        let expression = self.parse_expression(token)?;

        let mut token = self.next_token()?;
        while !token.is_eof_or(&TokenType::Semicolon) {
            token = self.next_token()?;
        }
        Ok(Statement::Expression(expression))
    }

    fn parse_expression(&mut self, token: TokenType) -> Result<Expression> {
        let left_term = self.parse_term(token)?;

        // TODO: op precedence is not implemented
        match self.parse_binary_op()? {
            Some(binary_op) => Ok(Expression::new_binary_op(left_term, binary_op)),
            None => Ok(Expression::new(left_term)),
        }
    }

    fn parse_subroutine_call(
        &mut self,
        identifier_token: IdentifierToken,
    ) -> Result<SubroutineCall> {
        let (parent_name, token) = if self.peek_token_is(TokenType::Dot)? {
            self.next_token()?;
            (Some(identifier_token), self.next_token()?)
        } else {
            (None, TokenType::Identifier(identifier_token))
        };

        let subroutine_name = if let TokenType::Identifier(identifier_token) = token {
            identifier_token
        } else {
            return Err(anyhow!("unexpected syntax of subroutine call: {:?}", token));
        };

        let token = self.next_token()?;
        if !token.is(&TokenType::Lparen) {
            return Err(anyhow!("unexpected syntax of subroutine call: {:?}", token));
        }

        let mut token = self.next_token()?;
        let mut expressions = vec![];
        while !token.is_eof_or(&TokenType::Rparen) {
            expressions.push(self.parse_expression(token)?);

            if self.peek_token_is(TokenType::Comma)? {
                self.next_token()?;
                token = self.next_token()?;
            } else if self.peek_token_is(TokenType::Rparen)? {
                token = self.next_token()?;
            } else {
                return Err(anyhow!(
                    "unexpected syntax of subroutine call: {:?}",
                    self.peek_token()?
                ));
            }
        }

        match parent_name {
            Some(parent_name) => Ok(SubroutineCall::new_parent_subroutine_call(
                parent_name,
                subroutine_name,
                expressions,
            )),
            None => Ok(SubroutineCall::new(subroutine_name, expressions)),
        }
    }

    fn parse_var_name(&mut self, identifier_token: IdentifierToken) -> Result<Term> {
        let identifier_token = identifier_token;

        if self.peek_token_is(TokenType::Lbracket)? {
            self.next_token()?;
            let token = self.next_token()?;
            let expression = self.parse_expression(token)?;

            if !self.peek_token_is(TokenType::Rbracket)? {
                return Err(anyhow!(
                    "unexpected syntax of varName: {:?}",
                    self.peek_token()?
                ));
            }

            self.next_token()?;

            Ok(Term::VarName(identifier_token, Some(Box::new(expression))))
        } else {
            Ok(Term::VarName(identifier_token, None))
        }
    }

    fn parse_expression_term(&mut self) -> Result<Term> {
        let token = self.next_token()?;
        let mut expression = self.parse_expression(token)?;

        if !self.peek_token_is(TokenType::Rparen)? {
            expression = match self.parse_binary_op()? {
                Some(binary_op) => {
                    Expression::new_binary_op(Term::Expresssion(Box::new(expression)), binary_op)
                }
                None => {
                    return Err(anyhow!(
                        "unexpected syntax of expression term: {:?}",
                        self.peek_token()?
                    ))
                }
            };
        }

        self.next_token()?;

        Ok(Term::Expresssion(Box::new(expression)))
    }

    fn parse_unary_op_constant(&mut self, op: TokenType) -> Result<Term> {
        let token = self.next_token()?;
        let term = self.parse_term(token)?;

        Ok(Term::UnaryOp(UnaryOpToken::new(op), Box::new(term)))
    }

    fn parse_term(&mut self, token: TokenType) -> Result<Term> {
        match token {
            TokenType::Identifier(identifier_token) => {
                if self.peek_token_is(TokenType::Lparen)? || self.peek_token_is(TokenType::Dot)? {
                    Ok(Term::SubroutineCall(
                        self.parse_subroutine_call(identifier_token)?,
                    ))
                } else {
                    self.parse_var_name(identifier_token)
                }
            }
            TokenType::Number(num) => Ok(Term::IntegerConstant(num)),
            TokenType::String(str_value) => Ok(Term::StringConstant(str_value)),
            TokenType::Keyword(keyword) => Ok(Term::KeywordConstant(KeywordConstant::new(keyword))),
            TokenType::Lparen => self.parse_expression_term(),
            TokenType::Minus | TokenType::Tilde => self.parse_unary_op_constant(token),
            _ => {
                return Err(anyhow!(
                    "unexpected token is passed to get_prefix_parse_function: {:?}",
                    token
                ))
            }
        }
    }

    fn parse_binary_op(&mut self) -> Result<Option<BinaryOp>> {
        if BinaryOpToken::is_binary_op_token_type(&self.peek_token()?) {
            let token = self.next_token()?;
            let op = BinaryOpToken::new(token);

            let token = self.next_token()?;
            let right_term = self.parse_term(token)?;

            Ok(Some(BinaryOp::new(op, right_term)))
        } else {
            Ok(None)
        }
    }

    fn peek_token_is(&mut self, token: TokenType) -> Result<bool> {
        Ok(self.peek_token()? == token)
    }
}

#[cfg(test)]
mod tests {
    use crate::analyzer::parser::*;
    use std::io::Cursor;

    struct FixedLabelGenerator(String);
    impl FixedLabelGenerator {
        fn new<S: Into<String>>(label: S) -> Self {
            Self(label.into())
        }
    }
    impl LabelGenerator for FixedLabelGenerator {
        fn generate(&self) -> String {
            self.0.clone()
        }
    }

    fn parse_ast_elements(tokenizer: &mut JackTokenizer) -> Vec<Statement> {
        let mut parser = Parser::new(tokenizer);

        let mut statements = vec![];
        let mut token = parser.next_token().unwrap();
        while parser.tokenizer.has_more_tokens() {
            statements.push(parser.parse_statement(token).unwrap());
            token = parser.next_token().unwrap();
        }
        statements
    }

    #[test]
    fn program() {
        let source = "
        class Main {
            static boolean test; // line comment
            /*
            multi line comment
              */
            function void main() {
                // line comment2
                var SquareGame game;
                var int num;
                let game = game;
                do game.run();
                return;
            }

            method void hoge(int a, boolean b) {
                return;
            }
        }
        "
        .as_bytes();

        let mut tokenizer = JackTokenizer::new(Cursor::new(&source));
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse_program().unwrap();

        assert_eq!(
            actual,
            Program::new(
                IdentifierToken::new("Main"),
                vec![ClassVarDec::new(
                    TokenType::Keyword(Keyword::Static),
                    ClassTypeToken::new(TokenType::Keyword(Keyword::Boolean)),
                    IdentifierToken::new("test"),
                    vec![],
                )],
                vec![
                    SubroutineDec::new(
                        IdentifierToken::new("Main"),
                        TokenType::Keyword(Keyword::Function),
                        None,
                        IdentifierToken::new("main"),
                        vec![],
                        vec![
                            VarDec::new(
                                ClassTypeToken::new(TokenType::Identifier(IdentifierToken::new(
                                    "SquareGame"
                                ))),
                                IdentifierToken::new("game"),
                                vec![],
                            ),
                            VarDec::new(
                                ClassTypeToken::new(TokenType::Keyword(Keyword::Int)),
                                IdentifierToken::new("num"),
                                vec![],
                            )
                        ],
                        vec![
                            Statement::Let(
                                IdentifierToken::new("game"),
                                None,
                                Expression::new(Term::VarName(IdentifierToken::new("game"), None)),
                            ),
                            Statement::Do(SubroutineCall::new_parent_subroutine_call(
                                IdentifierToken::new("game"),
                                IdentifierToken::new("run"),
                                vec![],
                            )),
                            Statement::Return(None),
                        ],
                    ),
                    SubroutineDec::new(
                        IdentifierToken::new("Main"),
                        TokenType::Keyword(Keyword::Method),
                        None,
                        IdentifierToken::new("hoge"),
                        vec![
                            (
                                ClassTypeToken::new(TokenType::Keyword(Keyword::Int)),
                                IdentifierToken::new("a"),
                            ),
                            (
                                ClassTypeToken::new(TokenType::Keyword(Keyword::Boolean)),
                                IdentifierToken::new("b"),
                            )
                        ],
                        vec![],
                        vec![Statement::Return(None),],
                    )
                ],
            )
        )
    }

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
        let actual = parse_ast_elements(&mut tokenizer);

        assert_eq!(actual.len(), 6);
        assert_eq!(
            actual,
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
        let actual = parse_ast_elements(&mut tokenizer);

        assert_eq!(actual.len(), 4);
        assert_eq!(
            actual,
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
        let actual = parse_ast_elements(&mut tokenizer);

        assert_eq!(actual.len(), 3);
        assert_eq!(
            actual,
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
        let actual = parse_ast_elements(&mut tokenizer);

        assert_eq!(actual.len(), 1);
        assert_eq!(
            actual,
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
        let mut parser = Parser::new_by_label_generator(
            &mut tokenizer,
            Box::new(FixedLabelGenerator::new("Label")),
        );

        let mut actual = vec![];
        let mut token = parser.next_token().unwrap();
        while parser.tokenizer.has_more_tokens() {
            actual.push(parser.parse_statement(token).unwrap());
            token = parser.next_token().unwrap();
        }

        assert_eq!(actual.len(), 2);
        assert_eq!(
            actual,
            vec![
                Statement::While(
                    Expression::new(Term::KeywordConstant(KeywordConstant::new(Keyword::True))),
                    "Label".to_string(),
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
                    "Label".to_string(),
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
        let mut parser = Parser::new_by_label_generator(
            &mut tokenizer,
            Box::new(FixedLabelGenerator::new("Label")),
        );

        let mut actual = vec![];
        let mut token = parser.next_token().unwrap();
        while parser.tokenizer.has_more_tokens() {
            actual.push(parser.parse_statement(token).unwrap());
            token = parser.next_token().unwrap();
        }

        assert_eq!(actual.len(), 2);
        assert_eq!(
            actual,
            vec![
                Statement::If(
                    Expression::new(Term::KeywordConstant(KeywordConstant::new(Keyword::True))),
                    "Label".to_string(),
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
                    "Label".to_string(),
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
        let actual = parse_ast_elements(&mut tokenizer);

        assert_eq!(actual.len(), 1);
        assert_eq!(
            actual,
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
        let actual = parse_ast_elements(&mut tokenizer);

        assert_eq!(actual.len(), 3);
        assert_eq!(
            actual,
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
        let actual = parse_ast_elements(&mut tokenizer);

        assert_eq!(actual.len(), 1);
        assert_eq!(
            actual,
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
        let actual = parse_ast_elements(&mut tokenizer);

        assert_eq!(actual.len(), 1);
        assert_eq!(
            actual,
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
        let actual = parse_ast_elements(&mut tokenizer);

        assert_eq!(actual.len(), 3);
        assert_eq!(
            actual,
            vec![
                Statement::Expression(Expression::new(Term::KeywordConstant(
                    KeywordConstant::new(Keyword::This)
                ))),
                Statement::Expression(Expression::new(Term::KeywordConstant(
                    KeywordConstant::new(Keyword::Null)
                ))),
                Statement::Expression(Expression::new(Term::KeywordConstant(
                    KeywordConstant::new(Keyword::False)
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
        let actual = parse_ast_elements(&mut tokenizer);

        assert_eq!(actual.len(), 4);
        assert_eq!(
            actual,
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
        let actual = parse_ast_elements(&mut tokenizer);

        assert_eq!(actual.len(), 2);
        assert_eq!(
            actual,
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
        let actual = parse_ast_elements(&mut tokenizer);

        assert_eq!(actual.len(), 2);
        assert_eq!(
            actual,
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
