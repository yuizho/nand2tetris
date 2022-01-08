use super::token;

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self);
}

pub trait Expression: Node {
    fn expession_node(&self);
}

pub struct Program<T: Statement> {
    statements: Vec<T>,
}
impl<T: Statement> Node for Program<T> {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }
}

pub struct LetStatement {
    token: token::TokenType,
    identifier: Identifier,
    value: dyn Expression,
}
impl Node for LetStatement {
    fn token_literal(&self) -> String {
        // TODO: needs implement
        "".to_string()
    }
}
impl Statement for LetStatement {
    fn statement_node(&self) {}
}

pub struct Identifier {
    token: token::TokenType,
    value: String,
}
impl Node for Identifier {
    fn token_literal(&self) -> String {
        // TODO: needs implement
        "".to_string()
    }
}
impl Statement for Identifier {
    fn statement_node(&self) {}
}
