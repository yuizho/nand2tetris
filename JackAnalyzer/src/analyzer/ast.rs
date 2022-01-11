use super::token;

#[derive(PartialEq, Debug)]
pub enum Node {
    Program(Vec<Statement>),
    Statement(Statement),
    Expression(Expression),
}
impl Node {
    pub fn token_literal(&self) -> String {
        match self {
            Self::Program(statements) => {
                if statements.len() > 0 {
                    statements[0].token_literal()
                } else {
                    "".to_string()
                }
            }
            Self::Statement(statement) => statement.token_literal(),
            Self::Expression(expression) => expression.token_literal(),
        }
    }

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
    LetStatement(token::TokenType, Expression, Expression),
    ReturnStatement(token::TokenType, Option<Expression>),
}
impl Statement {
    pub fn statement_node(&self) {}
    pub fn token_literal(&self) -> String {
        match self {
            Self::LetStatement(token, _, _) => token.get_literal(),
            Self::ReturnStatement(token, _) => token.get_literal(),
        }
    }
    pub fn to_xml(&self) -> String {
        match self {
            Self::LetStatement(_, identifier, expression) => {
                format!(
                    "<letStatement>\n  {}\n  {}\n  {}\n  {}\n  {}\n</letStatement>",
                    "<keyword> let </keyword>",
                    identifier.to_xml(),
                    "<symbol> = </symbol>",
                    format!(
                        "<expression>\n    <term>\n      {}\n    </term>\n  </expression>",
                        expression.to_xml()
                    ),
                    "<symbol> ; </symbol>"
                )
            }
            Self::ReturnStatement(_, _) => "return".to_string(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Identifier(token::TokenType),
    Dummy,
}
impl Expression {
    pub fn expession_node(&self) {}
    pub fn token_literal(&self) -> String {
        match self {
            Self::Identifier(token) => token.get_literal(),
            Self::Dummy => "".to_string(),
        }
    }

    pub fn to_xml(&self) -> String {
        match self {
            Self::Identifier(token) => {
                format!("<identifier> {} </identifier>", token.get_literal())
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
    fn to_xml() {
        let program = Node::Program(vec![Statement::LetStatement(
            TokenType::KEYWORD(Keyword::LET),
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
