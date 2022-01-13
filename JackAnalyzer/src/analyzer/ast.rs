use super::token::{IdentifierToken, Keyword, Token, TokenType};

#[derive(PartialEq, Debug)]
pub enum Node {
    Program(Vec<Statement>),
    Statement(Statement),
    Expression(Expression),
}
impl Node {
    pub fn to_xml(&self) -> String {
        match self {
            Self::Program(staements) => staements
                .iter()
                .map(|s| s.to_xml())
                .collect::<Vec<_>>()
                .join("\n"),
            Self::Statement(statement) => statement.to_xml(),
            Self::Expression(expression) => expression.to_xml(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    LetStatement(IdentifierToken, Expression),
    ReturnStatement(Option<Expression>),
    ExpressionStatement(Expression),
}
impl Statement {
    pub fn statement_node(&self) {}
    pub fn to_xml(&self) -> String {
        match self {
            Self::LetStatement(identifier, expression) => {
                format!(
                    "<letStatement>\n  {}\n  {}\n  {}\n  {}\n  {}\n</letStatement>",
                    TokenType::KEYWORD(Keyword::LET).get_xml_tag(),
                    identifier.get_xml_tag(),
                    TokenType::ASSIGN.get_xml_tag(),
                    format!(
                        "<expression>\n    <term>\n      {}\n    </term>\n  </expression>",
                        expression.to_xml()
                    ),
                    TokenType::SEMICOLON.get_xml_tag()
                )
            }

            Self::ReturnStatement(Some(expression)) => format!(
                "<returnStatement>\n  {}\n  {}\n  {}\n</returnStatement>",
                TokenType::KEYWORD(Keyword::RETURN).get_xml_tag(),
                format!(
                    "<expression>\n    <term>\n      {}\n    </term>\n  </expression>",
                    expression.to_xml()
                ),
                TokenType::SEMICOLON.get_xml_tag()
            ),

            Self::ReturnStatement(None) => {
                format!(
                    "<returnStatement>\n  {}\n  {}\n</returnStatement>",
                    TokenType::KEYWORD(Keyword::RETURN).get_xml_tag(),
                    TokenType::SEMICOLON.get_xml_tag()
                )
            }

            Self::ExpressionStatement(expression) => expression.to_xml(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Identifier(IdentifierToken),
    IntegerConstant(i32),
    Dummy,
}
impl Expression {
    pub fn expession_node(&self) {}
    pub fn to_xml(&self) -> String {
        match self {
            Self::Identifier(token) => {
                format!("{}", token.get_xml_tag())
            }

            Self::IntegerConstant(num) => {
                format!("{}", num.get_xml_tag())
            }

            Self::Dummy => "dummy".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::analyzer::ast::*;
    use crate::analyzer::token::*;

    #[test]
    fn let_statement_to_xml() {
        let program = Node::Program(vec![Statement::LetStatement(
            IdentifierToken {
                identifier: "myVar".to_string(),
            },
            Expression::Identifier(IdentifierToken {
                identifier: "anotherVar".to_string(),
            }),
        )]);

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
    fn return_statement_no_identifier_to_xml() {
        let program = Node::Program(vec![Statement::ReturnStatement(None)]);

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
        let program = Node::Program(vec![Statement::ReturnStatement(Some(
            Expression::Identifier(IdentifierToken {
                identifier: "square".to_string(),
            }),
        ))]);

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
        let program = Node::Program(vec![Statement::ExpressionStatement(
            Expression::Identifier(IdentifierToken {
                identifier: "foo".to_string(),
            }),
        )]);

        assert_eq!(program.to_xml(), "<identifier> foo </identifier>")
    }

    #[test]
    fn integer_constant_expression_to_xml() {
        let program = Node::Program(vec![Statement::ExpressionStatement(
            Expression::IntegerConstant(10),
        )]);

        assert_eq!(program.to_xml(), "<integerConstant> 10 </integerConstant>")
    }
}
