use super::token::{IdentifierToken, Keyword, Token, TokenType};

pub trait Node {
    fn to_xml(&self) -> String;
}

pub struct Program {
    class_name: IdentifierToken,
    class_var_dec: Vec<ClassVarDec>,
    subroutine_dec: Vec<SubroutineDec>,
}
impl Program {
    pub fn new(
        class_name: IdentifierToken,
        class_var_dec: Vec<ClassVarDec>,
        subroutine_dec: Vec<SubroutineDec>,
    ) -> Self {
        Program {
            class_name,
            class_var_dec,
            subroutine_dec,
        }
    }
}
impl Node for Program {
    fn to_xml(&self) -> String {
        "".to_string()
    }
}

#[derive(PartialEq, Debug)]
pub struct ClassTypeToken {
    type_token: TokenType,
}
impl ClassTypeToken {
    pub fn new(type_token: TokenType) -> Self {
        let is_class_type = matches!(
            type_token,
            TokenType::Keyword(Keyword::Int)
                | TokenType::Keyword(Keyword::Char)
                | TokenType::Keyword(Keyword::Boolean)
                | TokenType::Identifier(_)
        );
        if is_class_type {
            ClassTypeToken { type_token }
        } else {
            panic!(
                "unexpected token type is used as class type: {:?}",
                type_token
            )
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct ClassVarDec {
    var_identifier: Keyword,
    var_type: ClassTypeToken,
    var_name: IdentifierToken,
    var_names: Vec<IdentifierToken>,
}
impl ClassVarDec {
    pub fn new(
        var_identifier: TokenType,
        var_type: ClassTypeToken,
        var_name: IdentifierToken,
        var_names: Vec<IdentifierToken>,
    ) -> Self {
        let var_identifier = match var_identifier {
            TokenType::Keyword(keyword) => match keyword {
                Keyword::Static | Keyword::Field => keyword,
                _ => panic!("unexpected keyword: {:?}", keyword),
            },
            _ => panic!("unexpected var_identifier: {:?}", var_identifier),
        };

        ClassVarDec {
            var_identifier,
            var_type,
            var_name,
            var_names,
        }
    }

    fn to_xml(&self) -> String {
        let class_var_names = if self.var_names.is_empty() {
            "".to_string()
        } else {
            format!(
                "\n{}",
                self.var_names
                    .iter()
                    .map(|i| format!("{}\n{}", TokenType::Comma.get_xml_tag(), i.get_xml_tag()))
                    .collect::<Vec<_>>()
                    .join("\n")
            )
        };

        format!(
            "<classVarDec>\n{}\n{}\n{}{}\n{}\n</classVarDec>",
            self.var_identifier.get_xml_tag(),
            self.var_type.type_token.get_xml_tag(),
            self.var_name.get_xml_tag(),
            class_var_names,
            TokenType::Semicolon.get_xml_tag(),
        )
    }
}

#[derive(PartialEq, Debug)]
pub struct SubroutineDec {
    subroutine_identifier: Keyword,
    return_type: Option<ClassTypeToken>,
    subroutine_name: IdentifierToken,
    parameters: Vec<(ClassTypeToken, IdentifierToken)>,
    var_dec: Vec<VarDec>,
    statements: Vec<Statement>,
}
impl SubroutineDec {
    pub fn new(
        subroutine_identifier: TokenType,
        return_type: Option<ClassTypeToken>,
        subroutine_name: IdentifierToken,
        parameters: Vec<(ClassTypeToken, IdentifierToken)>,
        var_dec: Vec<VarDec>,
        statements: Vec<Statement>,
    ) -> Self {
        let subroutine_identifier = match subroutine_identifier {
            TokenType::Keyword(keyword) => match keyword {
                Keyword::Constructor | Keyword::Function | Keyword::Method => keyword,
                _ => panic!("unexpected keyword: {:?}", keyword),
            },
            _ => panic!("unexpected var_identifier: {:?}", subroutine_identifier),
        };
        SubroutineDec {
            subroutine_identifier,
            return_type,
            subroutine_name,
            parameters,
            var_dec,
            statements,
        }
    }

    fn to_xml(&self) -> String {
        let parameters = if self.parameters.is_empty() {
            "".to_string()
        } else {
            format!(
                "\n{}",
                self.parameters
                    .iter()
                    .map(|(t, i)| format!("{}\n{}", t.type_token.get_xml_tag(), i.get_xml_tag()))
                    .collect::<Vec<_>>()
                    .join(&format!("\n{}\n", TokenType::Comma.get_xml_tag()))
            )
        };

        format!(
            "<subroutineDec>\n{}\n{}\n{}\n{}\n<subroutineBody>\n{}\n</subroutineBody>\n</subroutineDec>",
            self.subroutine_identifier.get_xml_tag(),
            match &self.return_type {
                Some(class_type) => class_type.type_token.get_xml_tag(),
                None => Keyword::Void.get_xml_tag(),
            },
            self.subroutine_name.get_xml_tag(),
            format!(
                "{}\n<parameterList>{}\n</parameterList>\n{}",
                TokenType::Lparen.get_xml_tag(),
                parameters,
                TokenType::Rparen.get_xml_tag()
            ),
            format!("{}\n{}\n<statements>\n{}\n</statements>\n{}",
            TokenType::Lbrace.get_xml_tag(),
            self
                .var_dec
                .iter()
                .map(|s| s.to_xml())
                .collect::<Vec<_>>()
                .join("\n"),
            self
                .statements
                .iter()
                .map(|s| s.to_xml())
                .collect::<Vec<_>>()
                .join("\n"),
            TokenType::Rbrace.get_xml_tag()),
        )
    }
}

#[derive(PartialEq, Debug)]
pub struct VarDec {
    var_type: ClassTypeToken,
    var_name: IdentifierToken,
    var_names: Vec<IdentifierToken>,
}
impl VarDec {
    pub fn new(
        var_type: ClassTypeToken,
        var_name: IdentifierToken,
        var_names: Vec<IdentifierToken>,
    ) -> Self {
        VarDec {
            var_type,
            var_name,
            var_names,
        }
    }

    fn to_xml(&self) -> String {
        let var_names = if self.var_names.is_empty() {
            "".to_string()
        } else {
            format!(
                "\n{}",
                self.var_names
                    .iter()
                    .map(|i| format!("{}\n{}", TokenType::Comma.get_xml_tag(), i.get_xml_tag()))
                    .collect::<Vec<_>>()
                    .join("\n")
            )
        };

        format!(
            "<varDec>\n{}\n{}\n{}{}\n{}\n</varDec>",
            Keyword::Var.get_xml_tag(),
            self.var_type.type_token.get_xml_tag(),
            self.var_name.get_xml_tag(),
            var_names,
            TokenType::Semicolon.get_xml_tag(),
        )
    }
}

#[derive(PartialEq, Debug)]
pub enum Class {
    ClassVarDec(ClassVarDec),
    SubroutineDec(SubroutineDec),
    VarDec(VarDec),
}
impl Node for Class {
    fn to_xml(&self) -> String {
        match self {
            Self::ClassVarDec(class_var_dec) => class_var_dec.to_xml(),

            Self::SubroutineDec(subroutine_dec) => subroutine_dec.to_xml(),

            Self::VarDec(var_dec) => var_dec.to_xml(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    Let(IdentifierToken, Option<Expression>, Expression),
    While(Expression, Vec<Statement>),
    If(Expression, Vec<Statement>, Option<Vec<Statement>>),
    Do(SubroutineCall),
    Return(Option<Expression>),
    Expression(Expression),
}
impl Node for Statement {
    fn to_xml(&self) -> String {
        match self {
            Self::Let(var_name, index_expression, expression) => {
                format!(
                    "<letStatement>\n{}\n{}\n{}{}\n{}\n{}\n</letStatement>",
                    TokenType::Keyword(Keyword::Let).get_xml_tag(),
                    var_name.get_xml_tag(),
                    match index_expression {
                        Some(exp) => {
                            format!(
                                "{}\n{}\n{}\n",
                                TokenType::Lbracket.get_xml_tag(),
                                exp.to_xml(),
                                TokenType::Rbracket.get_xml_tag()
                            )
                        }
                        None => "".to_string(),
                    },
                    TokenType::Assign.get_xml_tag(),
                    expression.to_xml(),
                    TokenType::Semicolon.get_xml_tag()
                )
            }

            Self::While(expression, statements) => {
                format!(
                    "<whileStatement>\n{}\n{}\n{}\n{}\n{}\n{}\n{}\n</whileStatement>",
                    Keyword::While.get_xml_tag(),
                    TokenType::Lparen.get_xml_tag(),
                    expression.to_xml(),
                    TokenType::Rparen.get_xml_tag(),
                    TokenType::Lbrace.get_xml_tag(),
                    format!(
                        "<statements>\n{}\n</statements>",
                        statements
                            .iter()
                            .map(|s| s.to_xml())
                            .collect::<Vec<_>>()
                            .join("\n")
                    ),
                    TokenType::Rbrace.get_xml_tag(),
                )
            }

            Self::If(expression, if_statements, else_statements) => {
                format!(
                    "<ifStatement>\n{}\n{}\n{}\n{}\n{}\n{}\n{}\n{}</ifStatement>",
                    Keyword::If.get_xml_tag(),
                    TokenType::Lparen.get_xml_tag(),
                    expression.to_xml(),
                    TokenType::Rparen.get_xml_tag(),
                    TokenType::Lbrace.get_xml_tag(),
                    format!(
                        "<statements>\n{}\n</statements>",
                        if_statements
                            .iter()
                            .map(|s| s.to_xml())
                            .collect::<Vec<_>>()
                            .join("\n")
                    ),
                    TokenType::Rbrace.get_xml_tag(),
                    match else_statements {
                        Some(statements) => format!(
                            "{}\n{}\n{}\n{}\n",
                            Keyword::Else.get_xml_tag(),
                            TokenType::Lbrace.get_xml_tag(),
                            format!(
                                "<statements>\n{}\n</statements>",
                                statements
                                    .iter()
                                    .map(|s| s.to_xml())
                                    .collect::<Vec<_>>()
                                    .join("\n")
                            ),
                            TokenType::Rbrace.get_xml_tag(),
                        ),
                        None => "".to_string(),
                    }
                )
            }

            Self::Return(Some(expression)) => format!(
                "<returnStatement>\n{}\n{}\n{}\n</returnStatement>",
                TokenType::Keyword(Keyword::Return).get_xml_tag(),
                expression.to_xml(),
                TokenType::Semicolon.get_xml_tag()
            ),

            Self::Do(subroutine_call) => format!(
                "<doStatement>\n{}\n{}\n{}\n</doStatement>",
                Keyword::Do.get_xml_tag(),
                subroutine_call.to_xml(),
                TokenType::Semicolon.get_xml_tag()
            ),

            Self::Return(None) => {
                format!(
                    "<returnStatement>\n{}\n{}\n</returnStatement>",
                    TokenType::Keyword(Keyword::Return).get_xml_tag(),
                    TokenType::Semicolon.get_xml_tag()
                )
            }

            Self::Expression(expression) => expression.to_xml(),
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
            TokenType::Tilde | TokenType::Minus => UnaryOpToken { token },
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
        matches!(
            token,
            TokenType::Plus
                | TokenType::Minus
                | TokenType::Asterisk
                | TokenType::Slash
                | TokenType::And
                | TokenType::Or
                | TokenType::Gt
                | TokenType::Lt
                | TokenType::Assign
        )
    }
}

#[derive(PartialEq, Debug)]
pub struct KeywordConstantToken {
    keyword: Keyword,
}
impl KeywordConstantToken {
    pub fn new(keyword: Keyword) -> Self {
        match keyword {
            Keyword::True | Keyword::False | Keyword::Null | Keyword::This => {
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
pub struct SubroutineCall {
    parent_name: Option<IdentifierToken>,
    subroutine_name: IdentifierToken,
    expressions: Vec<Expression>,
}
impl SubroutineCall {
    pub fn new(subroutine_name: IdentifierToken, expressions: Vec<Expression>) -> Self {
        SubroutineCall {
            parent_name: None,
            subroutine_name,
            expressions,
        }
    }

    pub fn new_parent_subroutine_call(
        parent_name: IdentifierToken,
        subroutine_name: IdentifierToken,
        expressions: Vec<Expression>,
    ) -> Self {
        SubroutineCall {
            parent_name: Some(parent_name),
            subroutine_name,
            expressions,
        }
    }

    pub fn to_xml(&self) -> String {
        format!(
            "{}{}\n{}\n<expressionList>\n{}</expressionList>\n{}",
            match &self.parent_name {
                Some(name) => format!("{}\n<symbol> . </symbol>\n", name.get_xml_tag()),
                None => "".to_string(),
            },
            self.subroutine_name.get_xml_tag(),
            TokenType::Lparen.get_xml_tag(),
            if self.expressions.is_empty() {
                "".to_string()
            } else {
                format!(
                    "{}\n",
                    self.expressions
                        .iter()
                        .map(|s| s.to_xml())
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            },
            TokenType::Rparen.get_xml_tag()
        )
    }
}

#[derive(PartialEq, Debug)]
pub enum Term {
    IntegerConstant(i32),
    StringConstant(String),
    KeywordConstant(KeywordConstantToken),
    VarName(IdentifierToken, Option<Box<Expression>>),
    Expresssion(Box<Expression>),
    SubroutineCall(SubroutineCall),
    UnaryOp(UnaryOpToken, Box<Term>),
}
impl Node for Term {
    fn to_xml(&self) -> String {
        let xml_tag = match self {
            Self::VarName(token, expression_opt) => match expression_opt {
                Some(expression) => format!(
                    "{}\n{}\n{}\n{}",
                    token.get_xml_tag(),
                    TokenType::Lbracket.get_xml_tag(),
                    expression.to_xml(),
                    TokenType::Rbracket.get_xml_tag(),
                ),
                None => token.get_xml_tag(),
            },

            Self::IntegerConstant(num) => num.get_xml_tag(),

            Self::StringConstant(s) => s.get_xml_tag(),

            Self::KeywordConstant(keyword) => keyword.keyword.get_xml_tag(),

            Self::Expresssion(expression) => format!(
                "{}\n{}\n{}",
                TokenType::Lparen.get_xml_tag(),
                expression.to_xml(),
                TokenType::Rparen.get_xml_tag()
            ),

            Self::SubroutineCall(subroutine_call) => subroutine_call.to_xml(),

            Self::UnaryOp(op, term) => format!("{}\n{}", op.token.get_xml_tag(), term.to_xml()),
        };
        format!("<term>\n{}\n</term>", xml_tag)
    }
}

#[cfg(test)]
mod tests {
    use crate::analyzer::ast::*;
    use crate::analyzer::token::*;

    #[test]
    fn class_var_dec_to_xml() {
        let class_var_dec = Class::ClassVarDec(ClassVarDec::new(
            TokenType::Keyword(Keyword::Static),
            ClassTypeToken::new(TokenType::Keyword(Keyword::Boolean)),
            IdentifierToken::new("test"),
            vec![],
        ));

        assert_eq!(
            class_var_dec.to_xml(),
            "<classVarDec>
<keyword> static </keyword>
<keyword> boolean </keyword>
<identifier> test </identifier>
<symbol> ; </symbol>
</classVarDec>"
        );
    }

    #[test]
    fn class_var_multiple_dec_to_xml() {
        let class_var_dec = Class::ClassVarDec(ClassVarDec::new(
            TokenType::Keyword(Keyword::Field),
            ClassTypeToken::new(TokenType::Keyword(Keyword::Int)),
            IdentifierToken::new("i"),
            vec![IdentifierToken::new("j1"), IdentifierToken::new("j2")],
        ));

        assert_eq!(
            class_var_dec.to_xml(),
            "<classVarDec>
<keyword> field </keyword>
<keyword> int </keyword>
<identifier> i </identifier>
<symbol> , </symbol>
<identifier> j1 </identifier>
<symbol> , </symbol>
<identifier> j2 </identifier>
<symbol> ; </symbol>
</classVarDec>"
        );
    }

    #[test]
    fn var_dec_to_xml() {
        let var_dec = Class::VarDec(VarDec::new(
            ClassTypeToken::new(TokenType::Identifier(IdentifierToken::new("String"))),
            IdentifierToken::new("s"),
            vec![],
        ));

        assert_eq!(
            var_dec.to_xml(),
            "<varDec>
<keyword> var </keyword>
<identifier> String </identifier>
<identifier> s </identifier>
<symbol> ; </symbol>
</varDec>"
        );
    }

    #[test]
    fn no_parameter_subroutine_dec_to_xml() {
        let subroutine_dec = Class::SubroutineDec(SubroutineDec::new(
            TokenType::Keyword(Keyword::Method),
            None,
            IdentifierToken::new("main"),
            vec![],
            vec![VarDec::new(
                ClassTypeToken::new(TokenType::Identifier(IdentifierToken::new("SquareGame"))),
                IdentifierToken::new("game"),
                vec![],
            )],
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
        ));

        assert_eq!(
            subroutine_dec.to_xml(),
            "<subroutineDec>
<keyword> method </keyword>
<keyword> void </keyword>
<identifier> main </identifier>
<symbol> ( </symbol>
<parameterList>
</parameterList>
<symbol> ) </symbol>
<subroutineBody>
<symbol> { </symbol>
<varDec>
<keyword> var </keyword>
<identifier> SquareGame </identifier>
<identifier> game </identifier>
<symbol> ; </symbol>
</varDec>
<statements>
<letStatement>
<keyword> let </keyword>
<identifier> game </identifier>
<symbol> = </symbol>
<expression>
<term>
<identifier> game </identifier>
</term>
</expression>
<symbol> ; </symbol>
</letStatement>
<doStatement>
<keyword> do </keyword>
<identifier> game </identifier>
<symbol> . </symbol>
<identifier> run </identifier>
<symbol> ( </symbol>
<expressionList>
</expressionList>
<symbol> ) </symbol>
<symbol> ; </symbol>
</doStatement>
<returnStatement>
<keyword> return </keyword>
<symbol> ; </symbol>
</returnStatement>
</statements>
<symbol> } </symbol>
</subroutineBody>
</subroutineDec>"
        );
    }

    #[test]
    fn parameter_subroutine_dec_to_xml() {
        let subroutine_dec = Class::SubroutineDec(SubroutineDec::new(
            TokenType::Keyword(Keyword::Method),
            Some(ClassTypeToken::new(TokenType::Keyword(Keyword::Boolean))),
            IdentifierToken::new("main"),
            vec![
                (
                    ClassTypeToken::new(TokenType::Keyword(Keyword::Boolean)),
                    IdentifierToken::new("b"),
                ),
                (
                    ClassTypeToken::new(TokenType::Keyword(Keyword::Char)),
                    IdentifierToken::new("c"),
                ),
            ],
            vec![VarDec::new(
                ClassTypeToken::new(TokenType::Identifier(IdentifierToken::new("SquareGame"))),
                IdentifierToken::new("game"),
                vec![],
            )],
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
        ));

        assert_eq!(
            subroutine_dec.to_xml(),
            "<subroutineDec>
<keyword> method </keyword>
<keyword> boolean </keyword>
<identifier> main </identifier>
<symbol> ( </symbol>
<parameterList>
<keyword> boolean </keyword>
<identifier> b </identifier>
<symbol> , </symbol>
<keyword> char </keyword>
<identifier> c </identifier>
</parameterList>
<symbol> ) </symbol>
<subroutineBody>
<symbol> { </symbol>
<varDec>
<keyword> var </keyword>
<identifier> SquareGame </identifier>
<identifier> game </identifier>
<symbol> ; </symbol>
</varDec>
<statements>
<letStatement>
<keyword> let </keyword>
<identifier> game </identifier>
<symbol> = </symbol>
<expression>
<term>
<identifier> game </identifier>
</term>
</expression>
<symbol> ; </symbol>
</letStatement>
<doStatement>
<keyword> do </keyword>
<identifier> game </identifier>
<symbol> . </symbol>
<identifier> run </identifier>
<symbol> ( </symbol>
<expressionList>
</expressionList>
<symbol> ) </symbol>
<symbol> ; </symbol>
</doStatement>
<returnStatement>
<keyword> return </keyword>
<symbol> ; </symbol>
</returnStatement>
</statements>
<symbol> } </symbol>
</subroutineBody>
</subroutineDec>"
        );
    }

    #[test]
    fn var_multiple_dec_to_xml() {
        let var_dec = Class::VarDec(VarDec::new(
            ClassTypeToken::new(TokenType::Keyword(Keyword::Int)),
            IdentifierToken::new("i"),
            vec![IdentifierToken::new("j")],
        ));

        assert_eq!(
            var_dec.to_xml(),
            "<varDec>
<keyword> var </keyword>
<keyword> int </keyword>
<identifier> i </identifier>
<symbol> , </symbol>
<identifier> j </identifier>
<symbol> ; </symbol>
</varDec>"
        );
    }

    #[test]
    fn expression_with_binary_op() {
        let program = Statement::Expression(Expression::new_binary_op(
            Term::VarName(IdentifierToken::new("i"), None),
            BinaryOp::new(
                BinaryOpToken::new(TokenType::Plus),
                Term::IntegerConstant(2),
            ),
        ));

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
        let program = Statement::Let(
            IdentifierToken::new("myVar"),
            None,
            Expression {
                left_term: Term::VarName(IdentifierToken::new("anotherVar"), None),
                binary_op: None,
            },
        );

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
        let program = Statement::Let(
            IdentifierToken::new("myVar"),
            Some(Expression {
                left_term: Term::VarName(IdentifierToken::new("i"), None),
                binary_op: None,
            }),
            Expression {
                left_term: Term::VarName(IdentifierToken::new("anotherVar"), None),
                binary_op: None,
            },
        );

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
        let program = Statement::While(
            Expression::new(Term::KeywordConstant(KeywordConstantToken::new(
                Keyword::True,
            ))),
            vec![
                Statement::Let(
                    IdentifierToken::new("i"),
                    None,
                    Expression::new(Term::IntegerConstant(1)),
                ),
                Statement::Let(
                    IdentifierToken::new("j"),
                    None,
                    Expression::new(Term::IntegerConstant(2)),
                ),
            ],
        );

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
    fn if_statement_no_else_to_xml() {
        let program = Statement::If(
            Expression::new(Term::KeywordConstant(KeywordConstantToken::new(
                Keyword::True,
            ))),
            vec![
                Statement::Let(
                    IdentifierToken::new("i"),
                    None,
                    Expression::new(Term::IntegerConstant(1)),
                ),
                Statement::Let(
                    IdentifierToken::new("j"),
                    None,
                    Expression::new(Term::IntegerConstant(2)),
                ),
            ],
            None,
        );

        assert_eq!(
            program.to_xml(),
            "<ifStatement>
<keyword> if </keyword>
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
</ifStatement>"
        )
    }

    #[test]
    fn if_statement_has_else_to_xml() {
        let program = Statement::If(
            Expression::new(Term::KeywordConstant(KeywordConstantToken::new(
                Keyword::True,
            ))),
            vec![Statement::Let(
                IdentifierToken::new("i"),
                None,
                Expression::new(Term::IntegerConstant(1)),
            )],
            Some(vec![
                Statement::Let(
                    IdentifierToken::new("ii"),
                    None,
                    Expression::new(Term::IntegerConstant(1)),
                ),
                Statement::Let(
                    IdentifierToken::new("jj"),
                    None,
                    Expression::new(Term::IntegerConstant(2)),
                ),
            ]),
        );

        assert_eq!(
            program.to_xml(),
            "<ifStatement>
<keyword> if </keyword>
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
</statements>
<symbol> } </symbol>
<keyword> else </keyword>
<symbol> { </symbol>
<statements>
<letStatement>
<keyword> let </keyword>
<identifier> ii </identifier>
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
<identifier> jj </identifier>
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
</ifStatement>"
        )
    }

    #[test]
    fn do_statement_to_xml() {
        let program = Statement::Do(SubroutineCall::new_parent_subroutine_call(
            IdentifierToken::new("game"),
            IdentifierToken::new("run"),
            vec![],
        ));

        assert_eq!(
            program.to_xml(),
            "<doStatement>
<keyword> do </keyword>
<identifier> game </identifier>
<symbol> . </symbol>
<identifier> run </identifier>
<symbol> ( </symbol>
<expressionList>
</expressionList>
<symbol> ) </symbol>
<symbol> ; </symbol>
</doStatement>"
        )
    }

    #[test]
    fn return_statement_no_identifier_to_xml() {
        let program = Statement::Return(None);

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
        let program = Statement::Return(Some(Expression {
            left_term: Term::VarName(IdentifierToken::new("square"), None),
            binary_op: None,
        }));

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
        let program = Statement::Expression(Expression {
            left_term: Term::VarName(IdentifierToken::new("foo"), None),
            binary_op: None,
        });

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
        let program = Statement::Expression(Expression {
            left_term: Term::VarName(
                IdentifierToken::new("foo"),
                Some(Box::new(Expression::new(Term::IntegerConstant(1)))),
            ),
            binary_op: None,
        });

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
        let program = Statement::Expression(Expression {
            left_term: Term::StringConstant("str value!!".to_string()),
            binary_op: None,
        });

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
        let program = Statement::Expression(Expression::new(Term::KeywordConstant(
            KeywordConstantToken::new(Keyword::True),
        )));

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
        let program = Statement::Expression(Expression::new(Term::Expresssion(Box::new(
            Expression::new(Term::UnaryOp(
                UnaryOpToken::new(TokenType::Minus),
                Box::new(Term::VarName(IdentifierToken::new("i"), None)),
            )),
        ))));

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
        let program = Statement::Expression(Expression::new(Term::UnaryOp(
            UnaryOpToken::new(TokenType::Minus),
            Box::new(Term::IntegerConstant(1)),
        )));

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
        let program = vec![
            Statement::Expression(Expression::new(Term::UnaryOp(
                UnaryOpToken::new(TokenType::Minus),
                Box::new(Term::IntegerConstant(1)),
            ))),
            Statement::Expression(Expression::new(Term::UnaryOp(
                UnaryOpToken::new(TokenType::Tilde),
                Box::new(Term::IntegerConstant(10)),
            ))),
        ];

        assert_eq!(
            program
                .iter()
                .map(|s| s.to_xml())
                .collect::<Vec<_>>()
                .join("\n"),
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

    #[test]
    fn local_subroutine_call_no_param_expression_to_xml() {
        let program = Statement::Expression(Expression::new(Term::SubroutineCall(
            SubroutineCall::new(IdentifierToken::new("new"), vec![]),
        )));

        assert_eq!(
            program.to_xml(),
            "<expression>
<term>
<identifier> new </identifier>
<symbol> ( </symbol>
<expressionList>
</expressionList>
<symbol> ) </symbol>
</term>
</expression>"
        )
    }

    #[test]
    fn local_subroutine_call_with_param_expression_to_xml() {
        let program =
            Statement::Expression(Expression::new(Term::SubroutineCall(SubroutineCall::new(
                IdentifierToken::new("new"),
                vec![
                    Expression::new(Term::IntegerConstant(1)),
                    Expression::new_binary_op(
                        Term::IntegerConstant(2),
                        BinaryOp::new(
                            BinaryOpToken::new(TokenType::Plus),
                            Term::IntegerConstant(3),
                        ),
                    ),
                ],
            ))));

        assert_eq!(
            program.to_xml(),
            "<expression>
<term>
<identifier> new </identifier>
<symbol> ( </symbol>
<expressionList>
<expression>
<term>
<integerConstant> 1 </integerConstant>
</term>
</expression>
<expression>
<term>
<integerConstant> 2 </integerConstant>
</term>
<symbol> + </symbol>
<term>
<integerConstant> 3 </integerConstant>
</term>
</expression>
</expressionList>
<symbol> ) </symbol>
</term>
</expression>"
        )
    }

    #[test]
    fn subroutine_call_no_param_expression_to_xml() {
        let program = Statement::Expression(Expression::new(Term::SubroutineCall(
            SubroutineCall::new_parent_subroutine_call(
                IdentifierToken::new("SquareGame"),
                IdentifierToken::new("new"),
                vec![],
            ),
        )));

        assert_eq!(
            program.to_xml(),
            "<expression>
<term>
<identifier> SquareGame </identifier>
<symbol> . </symbol>
<identifier> new </identifier>
<symbol> ( </symbol>
<expressionList>
</expressionList>
<symbol> ) </symbol>
</term>
</expression>"
        )
    }

    #[test]
    fn subroutine_call_with_param_expression_to_xml() {
        let program = Statement::Expression(Expression::new(Term::SubroutineCall(
            SubroutineCall::new_parent_subroutine_call(
                IdentifierToken::new("SquareGame"),
                IdentifierToken::new("new"),
                vec![
                    Expression::new(Term::IntegerConstant(1)),
                    Expression::new_binary_op(
                        Term::IntegerConstant(2),
                        BinaryOp::new(
                            BinaryOpToken::new(TokenType::Plus),
                            Term::IntegerConstant(3),
                        ),
                    ),
                ],
            ),
        )));

        assert_eq!(
            program.to_xml(),
            "<expression>
<term>
<identifier> SquareGame </identifier>
<symbol> . </symbol>
<identifier> new </identifier>
<symbol> ( </symbol>
<expressionList>
<expression>
<term>
<integerConstant> 1 </integerConstant>
</term>
</expression>
<expression>
<term>
<integerConstant> 2 </integerConstant>
</term>
<symbol> + </symbol>
<term>
<integerConstant> 3 </integerConstant>
</term>
</expression>
</expressionList>
<symbol> ) </symbol>
</term>
</expression>"
        )
    }
}
