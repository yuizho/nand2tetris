use super::token::{Keyword, TokenType};

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
    LetStatement(Expression, Expression),
    ReturnStatement(Option<Expression>),
}
impl Statement {
    pub fn statement_node(&self) {}
    pub fn to_xml(&self) -> String {
        match self {
            Self::LetStatement(identifier, expression) => {
                format!(
                    "<letStatement>\n  {}\n  {}\n  {}\n  {}\n  {}\n</letStatement>",
                    TokenType::KEYWORD(Keyword::LET).get_xml_tag(),
                    identifier.to_xml(),
                    TokenType::ASSIGN.get_xml_tag(),
                    format!(
                        "<expression>\n    <term>\n      {}\n    </term>\n  </expression>",
                        expression.to_xml()
                    ),
                    TokenType::SEMICOLON.get_xml_tag()
                )
            }
            Self::ReturnStatement(_) => "return".to_string(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Identifier(TokenType),
    Dummy,
}
impl Expression {
    pub fn expession_node(&self) {}
    pub fn to_xml(&self) -> String {
        match self {
            Self::Identifier(token) => {
                format!("{}", token.get_xml_tag())
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
            Expression::Identifier(TokenType::IDNETIFIER("myVar".to_string())),
            Expression::Identifier(TokenType::IDNETIFIER("anotherVar".to_string())),
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
}
