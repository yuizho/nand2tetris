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
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    LetStatement(token::TokenType, Expression, Expression),
}
impl Statement {
    pub fn statement_node(&self) {}
    pub fn token_literal(&self) -> String {
        match self {
            Self::LetStatement(token, _, _) => token.get_literal(),
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
}
