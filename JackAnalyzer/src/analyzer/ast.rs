use super::token::{IdentifierToken, Keyword, Token, TokenType};

pub trait Node {
    fn to_xml(&self) -> String;
}

pub struct Program {
    pub statements: Vec<Statement>,
}
impl Node for Program {
    fn to_xml(&self) -> String {
        self.statements
            .iter()
            .map(|s| s.to_xml())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    LetStatement(IdentifierToken, Option<Expression>, Expression),
    WhileStatement(Expression, Vec<Statement>),
    ReturnStatement(Option<Expression>),
    ExpressionStatement(Expression),
}
impl Node for Statement {
    fn to_xml(&self) -> String {
        match self {
            Self::LetStatement(var_name, index_expression, expression) => {
                format!(
                    "<letStatement>\n{}\n{}\n{}{}\n{}\n{}\n</letStatement>",
                    TokenType::KEYWORD(Keyword::LET).get_xml_tag(),
                    var_name.get_xml_tag(),
                    match index_expression {
                        Some(exp) => {
                            format!(
                                "{}\n{}\n{}\n",
                                TokenType::LBRACKET.get_xml_tag(),
                                exp.to_xml(),
                                TokenType::RBRACKET.get_xml_tag()
                            )
                        }
                        None => "".to_string(),
                    },
                    TokenType::ASSIGN.get_xml_tag(),
                    format!("{}", expression.to_xml()),
                    TokenType::SEMICOLON.get_xml_tag()
                )
            }

            Self::WhileStatement(expression, statements) => {
                format!(
                    "<whileStatement>\n{}\n{}\n{}\n{}\n{}\n{}\n{}\n</whileStatement>",
                    Keyword::WHILE.get_xml_tag(),
                    TokenType::LPAREN.get_xml_tag(),
                    expression.to_xml(),
                    TokenType::RPAREN.get_xml_tag(),
                    TokenType::LBRACE.get_xml_tag(),
                    format!(
                        "<statements>\n{}\n</statements>",
                        statements
                            .iter()
                            .map(|s| s.to_xml())
                            .collect::<Vec<_>>()
                            .join("\n")
                    ),
                    TokenType::RBRACE.get_xml_tag(),
                )
            }

            Self::ReturnStatement(Some(expression)) => format!(
                "<returnStatement>\n{}\n{}\n{}\n</returnStatement>",
                TokenType::KEYWORD(Keyword::RETURN).get_xml_tag(),
                format!("{}", expression.to_xml()),
                TokenType::SEMICOLON.get_xml_tag()
            ),

            Self::ReturnStatement(None) => {
                format!(
                    "<returnStatement>\n{}\n{}\n</returnStatement>",
                    TokenType::KEYWORD(Keyword::RETURN).get_xml_tag(),
                    TokenType::SEMICOLON.get_xml_tag()
                )
            }

            Self::ExpressionStatement(expression) => expression.to_xml(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Expression {
    pub left_term: Term,
    binary_op: Option<BinaryOp>,
}
impl Expression {
    pub fn new(term: Term) -> Self {
        Expression {
            left_term: term,
            binary_op: None,
        }
    }

    pub fn new_binary_op(term: Term, binary_op: BinaryOp) -> Self {
        Expression {
            left_term: term,
            binary_op: Some(binary_op),
        }
    }
}
impl Node for Expression {
    fn to_xml(&self) -> String {
        let binary_op_tag = match &self.binary_op {
            Some(binary_op) => format!("{}\n", binary_op.to_xml()),
            None => "".to_string(),
        };
        format!(
            "<expression>\n{}\n{}</expression>",
            self.left_term.to_xml(),
            binary_op_tag
        )
    }
}

#[derive(PartialEq, Debug)]
pub struct BinaryOp {
    op_token: BinaryOpToken,
    right_term: Term,
}
impl BinaryOp {
    pub fn new(op: BinaryOpToken, term: Term) -> Self {
        BinaryOp {
            op_token: op,
            right_term: term,
        }
    }
}
impl Node for BinaryOp {
    fn to_xml(&self) -> String {
        format!(
            "{}\n{}",
            self.op_token.token.get_xml_tag(),
            self.right_term.to_xml()
        )
    }
}

#[derive(PartialEq, Debug)]
pub struct UnaryOpToken {
    token: TokenType,
}
impl UnaryOpToken {
    pub fn new(token: TokenType) -> Self {
        match token {
            TokenType::TILDE | TokenType::MINUS => UnaryOpToken { token },
            _ => panic!("unexpected token type is used as unary op: {:?}", token),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct BinaryOpToken {
    token: TokenType,
}
impl BinaryOpToken {
    pub fn new(token: TokenType) -> Self {
        if BinaryOpToken::is_binary_op_token_type(&token) {
            BinaryOpToken { token }
        } else {
            panic!("unexpected token type is used as binary op: {:?}", token)
        }
    }

    pub fn is_binary_op_token_type(token: &TokenType) -> bool {
        match token {
            TokenType::PLUS
            | TokenType::MINUS
            | TokenType::ASTERISK
            | TokenType::SLASH
            | TokenType::AND
            | TokenType::OR
            | TokenType::GT
            | TokenType::LT
            | TokenType::ASSIGN => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct KeywordConstantToken {
    keyword: Keyword,
}
impl KeywordConstantToken {
    pub fn new(keyword: Keyword) -> Self {
        match keyword {
            Keyword::TRUE | Keyword::FALSE | Keyword::NULL | Keyword::THIS => {
                KeywordConstantToken { keyword }
            }
            _ => panic!(
                "unexpected keyword is used as keyword ocnstant: {:?}",
                keyword
            ),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Term {
    IntegerConstant(i32),
    StringConstant(String),
    KeywordConstant(KeywordConstantToken),
    VarName(IdentifierToken, Option<Box<Expression>>),
    Expresssion(Box<Expression>),
    UnaryOp(UnaryOpToken, Box<Term>),
}
impl Node for Term {
    fn to_xml(&self) -> String {
        match self {
            Self::VarName(token, expression_opt) => match expression_opt {
                Some(expression) => format!(
                    "<term>\n{}\n{}\n{}\n{}\n</term>",
                    token.get_xml_tag(),
                    TokenType::LBRACKET.get_xml_tag(),
                    expression.to_xml(),
                    TokenType::RBRACKET.get_xml_tag(),
                ),
                None => format!("<term>\n{}\n</term>", token.get_xml_tag()),
            },

            Self::IntegerConstant(num) => {
                format!("<term>\n{}\n</term>", num.get_xml_tag())
            }

            Self::StringConstant(s) => {
                format!("<term>\n{}\n</term>", s.get_xml_tag())
            }

            Self::KeywordConstant(keyword) => {
                format!("<term>\n{}\n</term>", keyword.keyword.get_xml_tag())
            }

            Self::Expresssion(expression) => format!(
                "<term>\n{}\n{}\n{}\n</term>",
                TokenType::LPAREN.get_xml_tag(),
                expression.to_xml(),
                TokenType::RPAREN.get_xml_tag()
            ),

            Self::UnaryOp(op, term) => format!(
                "<term>\n{}\n{}\n</term>",
                op.token.get_xml_tag(),
                term.to_xml()
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::analyzer::ast::*;
    use crate::analyzer::token::*;

    #[test]
    fn expression_with_binary_op() {
        let program = Program {
            statements: vec![Statement::ExpressionStatement(Expression::new_binary_op(
                Term::VarName(IdentifierToken::new("i".to_string()), None),
                BinaryOp::new(
                    BinaryOpToken::new(TokenType::PLUS),
                    Term::IntegerConstant(2),
                ),
            ))],
        };

        assert_eq!(
            program.to_xml(),
            "<expression>
<term>
<identifier> i </identifier>
</term>
<symbol> + </symbol>
<term>
<integerConstant> 2 </integerConstant>
</term>
</expression>"
        )
    }

    #[test]
    fn let_statement_to_xml() {
        let program = Program {
            statements: vec![Statement::LetStatement(
                IdentifierToken::new("myVar".to_string()),
                None,
                Expression {
                    left_term: Term::VarName(IdentifierToken::new("anotherVar".to_string()), None),
                    binary_op: None,
                },
            )],
        };

        assert_eq!(
            program.to_xml(),
            "<letStatement>
<keyword> let </keyword>
<identifier> myVar </identifier>
<symbol> = </symbol>
<expression>
<term>
<identifier> anotherVar </identifier>
</term>
</expression>
<symbol> ; </symbol>
</letStatement>"
        )
    }

    #[test]
    fn let_statement_for_array_to_xml() {
        let program = Program {
            statements: vec![Statement::LetStatement(
                IdentifierToken::new("myVar".to_string()),
                Some(Expression {
                    left_term: Term::VarName(IdentifierToken::new("i".to_string()), None),
                    binary_op: None,
                }),
                Expression {
                    left_term: Term::VarName(IdentifierToken::new("anotherVar".to_string()), None),
                    binary_op: None,
                },
            )],
        };

        assert_eq!(
            program.to_xml(),
            "<letStatement>
<keyword> let </keyword>
<identifier> myVar </identifier>
<symbol> [ </symbol>
<expression>
<term>
<identifier> i </identifier>
</term>
</expression>
<symbol> ] </symbol>
<symbol> = </symbol>
<expression>
<term>
<identifier> anotherVar </identifier>
</term>
</expression>
<symbol> ; </symbol>
</letStatement>"
        )
    }

    #[test]
    fn while_statement_to_xml() {
        let program = Program {
            statements: vec![Statement::WhileStatement(
                Expression::new(Term::KeywordConstant(KeywordConstantToken::new(
                    Keyword::TRUE,
                ))),
                vec![
                    Statement::LetStatement(
                        IdentifierToken::new("i".to_string()),
                        None,
                        Expression::new(Term::IntegerConstant(1)),
                    ),
                    Statement::LetStatement(
                        IdentifierToken::new("j".to_string()),
                        None,
                        Expression::new(Term::IntegerConstant(2)),
                    ),
                ],
            )],
        };

        assert_eq!(
            program.to_xml(),
            "<whileStatement>
<keyword> while </keyword>
<symbol> ( </symbol>
<expression>
<term>
<keyword> true </keyword>
</term>
</expression>
<symbol> ) </symbol>
<symbol> { </symbol>
<statements>
<letStatement>
<keyword> let </keyword>
<identifier> i </identifier>
<symbol> = </symbol>
<expression>
<term>
<integerConstant> 1 </integerConstant>
</term>
</expression>
<symbol> ; </symbol>
</letStatement>
<letStatement>
<keyword> let </keyword>
<identifier> j </identifier>
<symbol> = </symbol>
<expression>
<term>
<integerConstant> 2 </integerConstant>
</term>
</expression>
<symbol> ; </symbol>
</letStatement>
</statements>
<symbol> } </symbol>
</whileStatement>"
        )
    }

    #[test]
    fn return_statement_no_identifier_to_xml() {
        let program = Program {
            statements: vec![Statement::ReturnStatement(None)],
        };

        assert_eq!(
            program.to_xml(),
            "<returnStatement>
<keyword> return </keyword>
<symbol> ; </symbol>
</returnStatement>"
        )
    }

    #[test]
    fn return_statement_that_has_identifier_to_xml() {
        let program = Program {
            statements: vec![Statement::ReturnStatement(Some(Expression {
                left_term: Term::VarName(IdentifierToken::new("square".to_string()), None),
                binary_op: None,
            }))],
        };

        assert_eq!(
            program.to_xml(),
            "<returnStatement>
<keyword> return </keyword>
<expression>
<term>
<identifier> square </identifier>
</term>
</expression>
<symbol> ; </symbol>
</returnStatement>"
        )
    }

    #[test]
    fn var_name_expression_to_xml() {
        let program = Program {
            statements: vec![Statement::ExpressionStatement(Expression {
                left_term: Term::VarName(IdentifierToken::new("foo".to_string()), None),
                binary_op: None,
            })],
        };

        assert_eq!(
            program.to_xml(),
            "<expression>
<term>
<identifier> foo </identifier>
</term>
</expression>"
        )
    }

    #[test]
    fn var_name_has_indexexpression_to_xml() {
        let program = Program {
            statements: vec![Statement::ExpressionStatement(Expression {
                left_term: Term::VarName(
                    IdentifierToken::new("foo".to_string()),
                    Some(Box::new(Expression::new(Term::IntegerConstant(1)))),
                ),
                binary_op: None,
            })],
        };

        assert_eq!(
            program.to_xml(),
            "<expression>
<term>
<identifier> foo </identifier>
<symbol> [ </symbol>
<expression>
<term>
<integerConstant> 1 </integerConstant>
</term>
</expression>
<symbol> ] </symbol>
</term>
</expression>"
        )
    }

    #[test]
    fn string_constant_expression_to_xml() {
        let program = Program {
            statements: vec![Statement::ExpressionStatement(Expression {
                left_term: Term::StringConstant("str value!!".to_string()),
                binary_op: None,
            })],
        };

        assert_eq!(
            program.to_xml(),
            "<expression>
<term>
<stringConstant> str value!! </stringConstant>
</term>
</expression>"
        )
    }

    #[test]
    fn keyword_constant_expression_to_xml() {
        let program = Program {
            statements: vec![Statement::ExpressionStatement(Expression::new(
                Term::KeywordConstant(KeywordConstantToken::new(Keyword::TRUE)),
            ))],
        };

        assert_eq!(
            program.to_xml(),
            "<expression>
<term>
<keyword> true </keyword>
</term>
</expression>"
        )
    }

    #[test]
    fn expression_term_to_xml() {
        let program = Program {
            statements: vec![Statement::ExpressionStatement(Expression::new(
                Term::Expresssion(Box::new(Expression::new(Term::UnaryOp(
                    UnaryOpToken::new(TokenType::MINUS),
                    Box::new(Term::VarName(IdentifierToken::new("i".to_string()), None)),
                )))),
            ))],
        };

        assert_eq!(
            program.to_xml(),
            "<expression>
<term>
<symbol> ( </symbol>
<expression>
<term>
<symbol> - </symbol>
<term>
<identifier> i </identifier>
</term>
</term>
</expression>
<symbol> ) </symbol>
</term>
</expression>"
        )
    }

    #[test]
    fn unary_op_expression_to_xml() {
        let program = Program {
            statements: vec![Statement::ExpressionStatement(Expression::new(
                Term::UnaryOp(
                    UnaryOpToken::new(TokenType::MINUS),
                    Box::new(Term::IntegerConstant(1)),
                ),
            ))],
        };

        assert_eq!(
            program.to_xml(),
            "<expression>
<term>
<symbol> - </symbol>
<term>
<integerConstant> 1 </integerConstant>
</term>
</term>
</expression>"
        )
    }

    #[test]
    fn integer_constant_expression_to_xml() {
        let program = Program {
            statements: vec![
                Statement::ExpressionStatement(Expression::new(Term::UnaryOp(
                    UnaryOpToken::new(TokenType::MINUS),
                    Box::new(Term::IntegerConstant(1)),
                ))),
                Statement::ExpressionStatement(Expression::new(Term::UnaryOp(
                    UnaryOpToken::new(TokenType::TILDE),
                    Box::new(Term::IntegerConstant(10)),
                ))),
            ],
        };

        assert_eq!(
            program.to_xml(),
            "<expression>
<term>
<symbol> - </symbol>
<term>
<integerConstant> 1 </integerConstant>
</term>
</term>
</expression>
<expression>
<term>
<symbol> ~ </symbol>
<term>
<integerConstant> 10 </integerConstant>
</term>
</term>
</expression>"
        )
    }
}
