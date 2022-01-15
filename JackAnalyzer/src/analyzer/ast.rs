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
    pub binary_op: Option<BinaryOp>,
}
impl Expression {
    pub fn new(term: Term) -> Self {
        Expression {
            left_term: term,
            binary_op: None,
        }
    }
}
impl Node for Expression {
    fn to_xml(&self) -> String {
        let binary_op_tag = match &self.binary_op {
            Some(binary_op) => binary_op.to_xml(),
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
    op_token: TokenType,
    right_term: Term,
}
impl Node for BinaryOp {
    fn to_xml(&self) -> String {
        // TODO: needs impl
        "".to_string()
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
pub enum Term {
    Identifier(IdentifierToken),
    IntegerConstant(i32),
    StringConstant(String),
    //BynaryOp(TokenType, Expression), // TODO: make OpToken
    UnaryOp(UnaryOpToken, Box<Term>),
}
impl Node for Term {
    fn to_xml(&self) -> String {
        match self {
            Self::Identifier(token) => {
                format!("<term>\n{}\n</term>", token.get_xml_tag())
            }

            Self::IntegerConstant(num) => {
                format!("<term>\n{}\n</term>", num.get_xml_tag())
            }

            Self::StringConstant(s) => {
                format!("<term>\n{}\n</term>", s.get_xml_tag())
            }

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
    fn let_statement_to_xml() {
        let program = Program {
            statements: vec![Statement::LetStatement(
                IdentifierToken {
                    identifier: "myVar".to_string(),
                },
                None,
                Expression {
                    left_term: Term::Identifier(IdentifierToken {
                        identifier: "anotherVar".to_string(),
                    }),
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
                IdentifierToken {
                    identifier: "myVar".to_string(),
                },
                Some(Expression {
                    left_term: Term::Identifier(IdentifierToken {
                        identifier: "i".to_string(),
                    }),
                    binary_op: None,
                }),
                Expression {
                    left_term: Term::Identifier(IdentifierToken {
                        identifier: "anotherVar".to_string(),
                    }),
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
                left_term: Term::Identifier(IdentifierToken {
                    identifier: "square".to_string(),
                }),
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
    fn identifier_expression_to_xml() {
        let program = Program {
            statements: vec![Statement::ExpressionStatement(Expression {
                left_term: Term::Identifier(IdentifierToken {
                    identifier: "foo".to_string(),
                }),
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
    fn unary_op_expression_to_xml() {
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
