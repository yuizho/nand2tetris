use super::symbol_table::{ClassAttribute, LocalAttribute, SymbolTable};
use super::token::{IdentifierToken, Keyword, Token, TokenType};
use super::vm::{ArthmeticCommand, Command, Segment, Subroutine, SubroutineType, VmClass};
use super::xml::Element;
use std::str::FromStr;

#[derive(PartialEq, Eq, Debug)]
pub struct Program {
    class_name: IdentifierToken,
    class_var_dec: Vec<ClassVarDec>,
    subroutine_dec: Vec<SubroutineDec>,
    class_symbol_table: SymbolTable<ClassAttribute>,
}
impl Program {
    pub fn new(
        class_name: IdentifierToken,
        class_var_dec: Vec<ClassVarDec>,
        subroutine_dec: Vec<SubroutineDec>,
    ) -> Self {
        let mut class_symbol_table = SymbolTable::<ClassAttribute>::new();
        for c in &class_var_dec {
            class_symbol_table.add_symbol(
                c.var_name.0.clone(),
                c.var_type.to_string(),
                ClassAttribute::from_str(&c.var_identifier.to_string()).unwrap(),
            );
            for alt in &c.alt_var_names {
                class_symbol_table.add_symbol(
                    alt.0.clone(),
                    c.var_type.to_string(),
                    ClassAttribute::from_str(&c.var_identifier.to_string()).unwrap(),
                );
            }
        }

        for s in &subroutine_dec {
            class_symbol_table.add_symbol(
                format!("{}.{}", class_name.0, s.subroutine_name.0),
                "subroutine".to_string(),
                ClassAttribute::from_str(&s.subroutine_identifier.to_string()).unwrap(),
            )
        }

        Program {
            class_name,
            class_var_dec,
            subroutine_dec,
            class_symbol_table,
        }
    }

    pub fn to_vm(&self) -> VmClass {
        VmClass::new(
            self.subroutine_dec
                .iter()
                .map(|s| s.to_vm(&self.class_symbol_table))
                .collect::<Vec<Subroutine>>(),
        )
    }

    pub fn to_xml(&self) -> Element {
        Element::new_elements(
            "class",
            vec![
                Keyword::Class.get_xml_tag(),
                self.class_name.get_xml_tag(),
                TokenType::Lbrace.get_xml_tag(),
                if self.class_var_dec.is_empty() {
                    Element::empty()
                } else {
                    Element::new_fragment(
                        self.class_var_dec
                            .iter()
                            .map(|v| v.to_xml())
                            .collect::<Vec<_>>(),
                    )
                },
                if self.subroutine_dec.is_empty() {
                    Element::empty()
                } else {
                    Element::new_fragment(
                        self.subroutine_dec
                            .iter()
                            .map(|v| v.to_xml(&self.class_symbol_table))
                            .collect::<Vec<_>>(),
                    )
                },
                TokenType::Rbrace.get_xml_tag(),
            ],
        )
    }
}

#[derive(PartialEq, Eq, Debug)]
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
impl ToString for ClassTypeToken {
    fn to_string(&self) -> String {
        use TokenType::*;
        let s = match &self.type_token {
            Keyword(kw) => kw.to_string(),
            Identifier(id) => id.0.clone(),
            _ => panic!("unreachable"),
        };
        s.to_string()
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct ClassVarDec {
    var_identifier: Keyword,
    var_type: ClassTypeToken,
    var_name: IdentifierToken,
    alt_var_names: Vec<IdentifierToken>,
}
impl ClassVarDec {
    pub fn new(
        var_identifier: TokenType,
        var_type: ClassTypeToken,
        var_name: IdentifierToken,
        alt_var_names: Vec<IdentifierToken>,
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
            alt_var_names,
        }
    }

    fn to_xml(&self) -> Element {
        let class_var_names = if self.alt_var_names.is_empty() {
            Element::empty()
        } else {
            Element::new_fragment(
                self.alt_var_names
                    .iter()
                    .map(|i| {
                        Element::new_fragment(vec![
                            TokenType::Comma.get_xml_tag(),
                            i.get_xml_tag_with_attr(vec![
                                ("category".to_string(), self.var_type.to_string()),
                                ("how".to_string(), "define".to_string()),
                                ("attribute".to_string(), self.var_identifier.to_string()),
                            ]),
                        ])
                    })
                    .collect::<Vec<_>>(),
            )
        };

        Element::new_elements(
            "classVarDec",
            vec![
                self.var_identifier.get_xml_tag(),
                self.var_type.type_token.get_xml_tag(),
                self.var_name.get_xml_tag_with_attr(vec![
                    ("category".to_string(), self.var_type.to_string()),
                    ("how".to_string(), "define".to_string()),
                    ("attribute".to_string(), self.var_identifier.to_string()),
                ]),
                class_var_names,
                TokenType::Semicolon.get_xml_tag(),
            ],
        )
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct SubroutineDec {
    class_name: IdentifierToken,
    subroutine_identifier: Keyword,
    return_type: Option<ClassTypeToken>,
    subroutine_name: IdentifierToken,
    parameters: Vec<(ClassTypeToken, IdentifierToken)>,
    var_dec: Vec<VarDec>,
    statements: Vec<Statement>,
    local_symbol_table: SymbolTable<LocalAttribute>,
}
impl SubroutineDec {
    pub fn new(
        class_name: IdentifierToken,
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

        let mut local_symbol_table = SymbolTable::<LocalAttribute>::new();
        // when the subroutine is method, "this" is treated as a first argument
        if subroutine_identifier == Keyword::Method {
            local_symbol_table.add_symbol(
                "this".to_string(),
                class_name.0.clone(),
                LocalAttribute::Argument,
            );
        }
        for (class_type, identifier) in &parameters {
            local_symbol_table.add_symbol(
                identifier.0.clone(),
                class_type.to_string(),
                LocalAttribute::Argument,
            );
        }
        for v in &var_dec {
            local_symbol_table.add_symbol(
                v.var_name.0.clone(),
                v.var_type.to_string(),
                LocalAttribute::Var,
            );
            for alt_var_name in &v.alt_var_names {
                local_symbol_table.add_symbol(
                    alt_var_name.0.clone(),
                    v.var_type.to_string(),
                    LocalAttribute::Var,
                );
            }
        }

        SubroutineDec {
            class_name,
            subroutine_identifier,
            return_type,
            subroutine_name,
            parameters,
            var_dec,
            statements,
            local_symbol_table,
        }
    }

    fn to_vm(&self, class_symbol_table: &SymbolTable<ClassAttribute>) -> Subroutine {
        let mut commands = vec![];
        for s in &self.statements {
            commands.append(&mut s.to_vm(class_symbol_table, &self.local_symbol_table))
        }

        let var_dec_count = self.var_dec.len()
            + self
                .var_dec
                .iter()
                .map(|v| v.alt_var_names.len())
                .sum::<usize>();

        let subroutine_type = match self.subroutine_identifier {
            Keyword::Constructor => {
                SubroutineType::Constructor(class_symbol_table.var_count(&ClassAttribute::Field))
            }
            Keyword::Function => SubroutineType::Function,
            Keyword::Method => SubroutineType::Method,
            _ => panic!("unreachable"),
        };

        Subroutine::new(
            self.class_name.0.clone(),
            subroutine_type,
            self.subroutine_name.0.clone(),
            var_dec_count,
            commands,
        )
    }

    fn to_xml(&self, class_symbol_table: &SymbolTable<ClassAttribute>) -> Element {
        let parameters = if self.parameters.is_empty() {
            Element::empty()
        } else {
            Element::new_joinned_fragment(
                self.parameters
                    .iter()
                    .map(|(t, i)| {
                        Element::new_fragment(vec![
                            t.type_token.get_xml_tag(),
                            i.get_xml_tag_with_attr(vec![
                                ("category".to_string(), t.to_string()),
                                ("how".to_string(), "define".to_string()),
                                (
                                    "attribute".to_string(),
                                    LocalAttribute::Argument.to_string(),
                                ),
                            ]),
                        ])
                    })
                    .collect::<Vec<_>>(),
                TokenType::Comma.get_xml_tag(),
            )
        };
        let vars = if self.var_dec.is_empty() {
            Element::empty()
        } else {
            Element::new_fragment(self.var_dec.iter().map(|s| s.to_xml()).collect::<Vec<_>>())
        };
        let statements = if self.statements.is_empty() {
            Element::empty()
        } else {
            Element::new_fragment(
                self.statements
                    .iter()
                    .map(|s| s.to_xml(class_symbol_table, &self.local_symbol_table))
                    .collect::<Vec<_>>(),
            )
        };

        Element::new_elements(
            "subroutineDec",
            vec![
                self.subroutine_identifier.get_xml_tag(),
                match &self.return_type {
                    Some(class_type) => class_type.type_token.get_xml_tag(),
                    None => Keyword::Void.get_xml_tag(),
                },
                self.subroutine_name.get_xml_tag(),
                Element::new_fragment(vec![
                    TokenType::Lparen.get_xml_tag(),
                    Element::new_element("parameterList", parameters),
                    TokenType::Rparen.get_xml_tag(),
                ]),
                Element::new_element(
                    "subroutineBody",
                    Element::new_fragment(vec![
                        TokenType::Lbrace.get_xml_tag(),
                        vars,
                        Element::new_element("statements", statements),
                        TokenType::Rbrace.get_xml_tag(),
                    ]),
                ),
            ],
        )
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct VarDec {
    var_type: ClassTypeToken,
    var_name: IdentifierToken,
    alt_var_names: Vec<IdentifierToken>,
}
impl VarDec {
    pub fn new(
        var_type: ClassTypeToken,
        var_name: IdentifierToken,
        alt_var_names: Vec<IdentifierToken>,
    ) -> Self {
        VarDec {
            var_type,
            var_name,
            alt_var_names,
        }
    }

    fn to_xml(&self) -> Element {
        let var_names = if self.alt_var_names.is_empty() {
            Element::empty()
        } else {
            Element::new_fragment(vec![
                TokenType::Comma.get_xml_tag(),
                Element::new_joinned_fragment(
                    self.alt_var_names
                        .iter()
                        .map(|i| {
                            i.get_xml_tag_with_attr(vec![
                                ("category".to_string(), self.var_type.to_string()),
                                ("how".to_string(), "define".to_string()),
                                ("attribute".to_string(), LocalAttribute::Var.to_string()),
                            ])
                        })
                        .collect::<Vec<_>>(),
                    TokenType::Comma.get_xml_tag(),
                ),
            ])
        };

        Element::new_elements(
            "varDec",
            vec![
                Keyword::Var.get_xml_tag(),
                self.var_type.type_token.get_xml_tag(),
                self.var_name.get_xml_tag_with_attr(vec![
                    ("category".to_string(), self.var_type.to_string()),
                    ("how".to_string(), "define".to_string()),
                    ("attribute".to_string(), LocalAttribute::Var.to_string()),
                ]),
                var_names,
                TokenType::Semicolon.get_xml_tag(),
            ],
        )
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum Statement {
    Let(IdentifierToken, Option<Expression>, Expression),
    While(Expression, String, Vec<Statement>),
    If(Expression, String, Vec<Statement>, Option<Vec<Statement>>),
    Do(SubroutineCall),
    Return(Option<Expression>),
    Expression(Expression),
}
impl Statement {
    fn to_vm(
        &self,
        class_symbol_table: &SymbolTable<ClassAttribute>,
        local_symbol_table: &SymbolTable<LocalAttribute>,
    ) -> Vec<Command> {
        use Statement::*;
        match self {
            // TODO: is array
            Let(identifier_token, None, expression) => {
                let mut result = vec![];

                result.append(&mut expression.to_vm(class_symbol_table, local_symbol_table));

                // TODO: needs refactoring
                let index = match local_symbol_table.index_of(&identifier_token.0) {
                    Some(index) => index,
                    None => class_symbol_table
                        .index_of(&identifier_token.0)
                        .unwrap_or_else(|| panic!("{} is not defined", identifier_token.0)),
                };

                if let Some(local_attr) = local_symbol_table.attr_of(&identifier_token.0) {
                    result.push(Command::Pop(Segment::from_local_attr(local_attr), *index));
                } else {
                    let class_attr = class_symbol_table
                        .attr_of(&identifier_token.0)
                        .unwrap_or_else(|| panic!("{} is not defined", identifier_token.0));
                    result.push(Command::Pop(Segment::from_class_attr(class_attr), *index));
                }

                result
            }
            While(expression, label_id, statements) => {
                let mut result = vec![];

                result.push(Command::Label(format!("{}-while-loop", label_id)));
                result.append(&mut expression.to_vm(class_symbol_table, local_symbol_table));
                result.push(Command::If(format!("{}-while-statements", label_id)));
                result.push(Command::GoTo(format!("{}-while-end", label_id)));
                result.push(Command::Label(format!("{}-while-statements", label_id)));
                statements.iter().for_each(|s| {
                    result.append(&mut s.to_vm(class_symbol_table, local_symbol_table))
                });
                result.push(Command::GoTo(format!("{}-while-loop", label_id)));
                result.push(Command::Label(format!("{}-while-end", label_id)));

                result
            }
            If(expression, label_id, if_statements, else_statements) => {
                let mut result = vec![];

                result.append(&mut expression.to_vm(class_symbol_table, local_symbol_table));
                result.push(Command::If(format!("{}-if-statements", label_id)));
                if let Some(else_statements) = else_statements {
                    else_statements.iter().for_each(|s| {
                        result.append(&mut s.to_vm(class_symbol_table, local_symbol_table))
                    });
                }
                result.push(Command::GoTo(format!("{}-if-end", label_id)));
                result.push(Command::Label(format!("{}-if-statements", label_id)));
                if_statements.iter().for_each(|s| {
                    result.append(&mut s.to_vm(class_symbol_table, local_symbol_table))
                });
                result.push(Command::Label(format!("{}-if-end", label_id)));

                result
            }
            Do(subroutine_call) => {
                let mut commands = subroutine_call.to_vm(class_symbol_table, local_symbol_table);
                commands.push(Command::Pop(Segment::Temp, 0));
                commands
            }
            Return(Some(expression)) => {
                let mut commands = expression.to_vm(class_symbol_table, local_symbol_table);
                commands.push(Command::Return);
                commands
            }
            Return(None) => {
                vec![Command::Push(Segment::Const, 0), Command::Return]
            }
            _ => panic!("needs to implement other variants"),
        }
    }

    fn to_xml(
        &self,
        class_symbol_table: &SymbolTable<ClassAttribute>,
        local_symbol_table: &SymbolTable<LocalAttribute>,
    ) -> Element {
        match self {
            Self::Let(var_name, index_expression, expression) => Element::new_elements(
                "letStatement",
                vec![
                    TokenType::Keyword(Keyword::Let).get_xml_tag(),
                    var_name.get_xml_tag(),
                    match index_expression {
                        Some(exp) => Element::new_fragment(vec![
                            TokenType::Lbracket.get_xml_tag(),
                            exp.to_xml(class_symbol_table, local_symbol_table),
                            TokenType::Rbracket.get_xml_tag(),
                        ]),
                        None => Element::empty(),
                    },
                    TokenType::Assign.get_xml_tag(),
                    expression.to_xml(class_symbol_table, local_symbol_table),
                    TokenType::Semicolon.get_xml_tag(),
                ],
            ),

            Self::While(expression, _, statements) => Element::new_elements(
                "whileStatement",
                vec![
                    Keyword::While.get_xml_tag(),
                    TokenType::Lparen.get_xml_tag(),
                    expression.to_xml(class_symbol_table, local_symbol_table),
                    TokenType::Rparen.get_xml_tag(),
                    TokenType::Lbrace.get_xml_tag(),
                    Element::new_elements(
                        "statements",
                        statements
                            .iter()
                            .map(|s| s.to_xml(class_symbol_table, local_symbol_table))
                            .collect::<Vec<_>>(),
                    ),
                    TokenType::Rbrace.get_xml_tag(),
                ],
            ),

            Self::If(expression, _, if_statements, else_statements) => {
                fn statements_to_xml(
                    v: &[Statement],
                    class_symbol_table: &SymbolTable<ClassAttribute>,
                    local_symbol_table: &SymbolTable<LocalAttribute>,
                ) -> Element {
                    if v.is_empty() {
                        Element::new_element("statements", Element::empty())
                    } else {
                        Element::new_elements(
                            "statements",
                            v.iter()
                                .map(|s| s.to_xml(class_symbol_table, local_symbol_table))
                                .collect::<Vec<_>>(),
                        )
                    }
                }

                Element::new_elements(
                    "ifStatement",
                    vec![
                        Keyword::If.get_xml_tag(),
                        TokenType::Lparen.get_xml_tag(),
                        expression.to_xml(class_symbol_table, local_symbol_table),
                        TokenType::Rparen.get_xml_tag(),
                        TokenType::Lbrace.get_xml_tag(),
                        statements_to_xml(if_statements, class_symbol_table, local_symbol_table),
                        TokenType::Rbrace.get_xml_tag(),
                        match else_statements {
                            Some(statements) => Element::new_fragment(vec![
                                Keyword::Else.get_xml_tag(),
                                TokenType::Lbrace.get_xml_tag(),
                                statements_to_xml(
                                    statements,
                                    class_symbol_table,
                                    local_symbol_table,
                                ),
                                TokenType::Rbrace.get_xml_tag(),
                            ]),
                            None => Element::empty(),
                        },
                    ],
                )
            }

            Self::Return(Some(expression)) => Element::new_elements(
                "returnStatement",
                vec![
                    TokenType::Keyword(Keyword::Return).get_xml_tag(),
                    expression.to_xml(class_symbol_table, local_symbol_table),
                    TokenType::Semicolon.get_xml_tag(),
                ],
            ),

            Self::Do(subroutine_call) => Element::new_elements(
                "doStatement",
                vec![
                    Keyword::Do.get_xml_tag(),
                    subroutine_call.to_xml(class_symbol_table, local_symbol_table),
                    TokenType::Semicolon.get_xml_tag(),
                ],
            ),

            Self::Return(None) => Element::new_elements(
                "returnStatement",
                vec![
                    TokenType::Keyword(Keyword::Return).get_xml_tag(),
                    TokenType::Semicolon.get_xml_tag(),
                ],
            ),

            Self::Expression(expression) => {
                expression.to_xml(class_symbol_table, local_symbol_table)
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
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

    fn to_vm(
        &self,
        class_symbol_table: &SymbolTable<ClassAttribute>,
        local_symbol_table: &SymbolTable<LocalAttribute>,
    ) -> Vec<Command> {
        let mut result = vec![];

        result.append(&mut self.left_term.to_vm(class_symbol_table, local_symbol_table));

        if let Some(binary_op) = &self.binary_op {
            result.append(&mut binary_op.to_vm(class_symbol_table, local_symbol_table));
        }

        result
    }

    fn to_xml(
        &self,
        class_symbol_table: &SymbolTable<ClassAttribute>,
        local_symbol_table: &SymbolTable<LocalAttribute>,
    ) -> Element {
        let binary_op_tag = match &self.binary_op {
            Some(binary_op) => binary_op.to_xml(class_symbol_table, local_symbol_table),
            None => Element::empty(),
        };
        Element::new_elements(
            "expression",
            vec![
                self.left_term
                    .to_xml(class_symbol_table, local_symbol_table),
                binary_op_tag,
            ],
        )
    }
}

#[derive(PartialEq, Eq, Debug)]
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

    fn to_vm(
        &self,
        class_symbol_table: &SymbolTable<ClassAttribute>,
        local_symbol_table: &SymbolTable<LocalAttribute>,
    ) -> Vec<Command> {
        let mut result = vec![];

        result.append(
            &mut self
                .right_term
                .to_vm(class_symbol_table, local_symbol_table),
        );

        result.push(self.op_token.to_vm());

        result
    }

    fn to_xml(
        &self,
        class_symbol_table: &SymbolTable<ClassAttribute>,
        local_symbol_table: &SymbolTable<LocalAttribute>,
    ) -> Element {
        Element::new_fragment(vec![
            self.op_token.0.get_xml_tag(),
            self.right_term
                .to_xml(class_symbol_table, local_symbol_table),
        ])
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct UnaryOpToken(TokenType);
impl UnaryOpToken {
    pub fn new(token: TokenType) -> Self {
        match token {
            TokenType::Tilde | TokenType::Minus => UnaryOpToken(token),
            _ => panic!("unexpected token type is used as unary op: {:?}", token),
        }
    }

    pub fn to_vm(&self) -> Command {
        match self.0 {
            TokenType::Tilde => Command::Arthmetic(ArthmeticCommand::Not),
            TokenType::Minus => Command::Arthmetic(ArthmeticCommand::Neg),
            _ => panic!("unreachable"),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct BinaryOpToken(TokenType);
impl BinaryOpToken {
    pub fn new(token: TokenType) -> Self {
        if BinaryOpToken::is_binary_op_token_type(&token) {
            BinaryOpToken(token)
        } else {
            panic!("unexpected token type is used as binary op: {:?}", token)
        }
    }

    pub fn is_binary_op_token_type(token: &TokenType) -> bool {
        use super::token::TokenType::*;
        matches!(
            token,
            Plus | Minus | Asterisk | Slash | And | Or | Gt | Lt | Assign
        )
    }

    pub fn to_vm(&self) -> Command {
        use super::token::TokenType::*;
        match self.0 {
            Plus => Command::Arthmetic(ArthmeticCommand::Add),
            Minus => Command::Arthmetic(ArthmeticCommand::Sub),
            Asterisk => Command::Call("Math".to_string(), "multiply".to_string(), 2),
            Slash => Command::Call("Math".to_string(), "divide".to_string(), 2),
            And => Command::Arthmetic(ArthmeticCommand::And),
            Or => Command::Arthmetic(ArthmeticCommand::Or),
            Gt => Command::Arthmetic(ArthmeticCommand::Gt),
            Lt => Command::Arthmetic(ArthmeticCommand::Lt),
            Assign => Command::Arthmetic(ArthmeticCommand::Eq),
            _ => panic!("unreachable "),
        }
    }
}

type IntegerConstant = i32;

type StringConstant = String;

#[derive(PartialEq, Eq, Debug)]
pub struct KeywordConstant(Keyword);
impl KeywordConstant {
    pub fn new(keyword: Keyword) -> Self {
        match keyword {
            Keyword::True | Keyword::False | Keyword::Null | Keyword::This => {
                KeywordConstant(keyword)
            }
            _ => panic!(
                "unexpected keyword is used as keyword ocnstant: {:?}",
                keyword
            ),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct SubroutineCall {
    parent_name: IdentifierToken,
    subroutine_name: IdentifierToken,
    expressions: Vec<Expression>,
}
impl SubroutineCall {
    pub fn new(
        parent_name: IdentifierToken,
        subroutine_name: IdentifierToken,
        expressions: Vec<Expression>,
    ) -> Self {
        SubroutineCall {
            parent_name,
            subroutine_name,
            expressions,
        }
    }

    fn to_vm(
        &self,
        class_symbol_table: &SymbolTable<ClassAttribute>,
        local_symbol_table: &SymbolTable<LocalAttribute>,
    ) -> Vec<Command> {
        // TODO: remove
        let mut result = vec![];

        let mut parameter_commands = self
            .expressions
            .iter()
            .flat_map(|e| e.to_vm(class_symbol_table, local_symbol_table))
            .collect::<Vec<_>>();

        // TODO refactoring
        let parent_name = &self.parent_name.0;
        if local_symbol_table.contains(parent_name) {
            // handle method of local variable
            result.push(Command::Push(
                Segment::Local,
                *local_symbol_table.index_of(parent_name).unwrap(),
            ));
            result.append(&mut parameter_commands);
            result.push(Command::Call(
                local_symbol_table.type_of(parent_name).unwrap().to_string(),
                self.subroutine_name.clone().0,
                self.expressions.len() + 1,
            ))
        } else if class_symbol_table.contains(parent_name) {
            // handle method of class field
            result.push(Command::Push(
                Segment::This,
                *class_symbol_table.index_of(parent_name).unwrap(),
            ));
            result.append(&mut parameter_commands);
            result.push(Command::Call(
                class_symbol_table.type_of(parent_name).unwrap().to_string(),
                self.subroutine_name.clone().0,
                self.expressions.len() + 1,
            ))
        } else if class_symbol_table
            .contains(&format!("{}.{}", parent_name, self.subroutine_name.0))
        {
            // handle self class's subroutine
            result.push(Command::Push(Segment::Pointer, 0));
            result.append(&mut parameter_commands);
            result.push(Command::Call(
                parent_name.clone(),
                self.subroutine_name.clone().0,
                self.expressions.len() + 1,
            ));
        } else {
            // other class's static function
            result.append(&mut parameter_commands);
            result.push(Command::Call(
                parent_name.clone(),
                self.subroutine_name.clone().0,
                self.expressions.len(),
            ));
        };

        result
    }

    pub fn to_xml(
        &self,
        class_symbol_table: &SymbolTable<ClassAttribute>,
        local_symbol_table: &SymbolTable<LocalAttribute>,
    ) -> Element {
        Element::new_fragment(vec![
            Element::new_fragment(vec![
                self.parent_name.get_xml_tag(),
                TokenType::Dot.get_xml_tag(),
            ]),
            self.subroutine_name.get_xml_tag(),
            TokenType::Lparen.get_xml_tag(),
            Element::new_element(
                "expressionList",
                if self.expressions.is_empty() {
                    Element::empty()
                } else {
                    Element::new_joinned_fragment(
                        self.expressions
                            .iter()
                            .map(|e| e.to_xml(class_symbol_table, local_symbol_table))
                            .collect::<Vec<_>>(),
                        TokenType::Comma.get_xml_tag(),
                    )
                },
            ),
            TokenType::Rparen.get_xml_tag(),
        ])
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum Term {
    IntegerConstant(IntegerConstant),
    StringConstant(StringConstant),
    KeywordConstant(KeywordConstant),
    VarName(IdentifierToken, Option<Box<Expression>>),
    Expresssion(Box<Expression>),
    SubroutineCall(SubroutineCall),
    UnaryOp(UnaryOpToken, Box<Term>),
}
impl Term {
    fn to_vm(
        &self,
        class_symbol_table: &SymbolTable<ClassAttribute>,
        local_symbol_table: &SymbolTable<LocalAttribute>,
    ) -> Vec<Command> {
        use Term::*;
        match self {
            IntegerConstant(num) => vec![Command::Push(Segment::Const, *num as usize)],
            KeywordConstant(keyword) => match keyword.0 {
                Keyword::True => vec![
                    Command::Push(Segment::Const, 1),
                    Command::Arthmetic(ArthmeticCommand::Neg),
                ],
                Keyword::False | Keyword::Null => vec![Command::Push(Segment::Const, 0)],
                Keyword::This => vec![Command::Push(Segment::Pointer, 0)],
                _ => panic!("unexpected keyword is passed: {:?}", keyword),
            },
            // TODO: needs to impl array
            VarName(token, None) => {
                let var_name = &token.0;
                if local_symbol_table.contains(var_name) {
                    vec![Command::Push(
                        Segment::from_local_attr(local_symbol_table.attr_of(var_name).unwrap()),
                        *local_symbol_table.index_of(var_name).unwrap(),
                    )]
                } else {
                    vec![Command::Push(
                        Segment::from_class_attr(class_symbol_table.attr_of(var_name).unwrap()),
                        *class_symbol_table.index_of(var_name).unwrap(),
                    )]
                }
            }
            Expresssion(expression) => expression.to_vm(class_symbol_table, local_symbol_table),
            SubroutineCall(subroutine_call) => {
                subroutine_call.to_vm(class_symbol_table, local_symbol_table)
            }
            UnaryOp(token, term) => {
                let mut result = vec![];
                result.append(&mut term.to_vm(class_symbol_table, local_symbol_table));
                result.push(token.to_vm());
                result
            }
            t => panic!("needs to implement other variants: {:?}", t),
        }
    }

    fn to_xml(
        &self,
        class_symbol_table: &SymbolTable<ClassAttribute>,
        local_symbol_table: &SymbolTable<LocalAttribute>,
    ) -> Element {
        let xml_tag = match self {
            Self::VarName(token, expression_opt) => {
                let name = &token.0;
                let attributes = vec![
                    (
                        "category".to_string(),
                        class_symbol_table
                            .type_of(name)
                            .or_else(|| local_symbol_table.type_of(name))
                            .unwrap()
                            .to_string(),
                    ),
                    ("how".to_string(), "use".to_string()),
                    (
                        "attribute".to_string(),
                        match class_symbol_table.attr_of(name) {
                            Some(attr) => attr.to_string(),
                            None => match local_symbol_table.attr_of(name) {
                                Some(attr) => attr.to_string(),
                                None => panic!("the variable is not defined: {}", name),
                            },
                        },
                    ),
                ];
                match expression_opt {
                    Some(expression) => Element::new_fragment(vec![
                        token.get_xml_tag_with_attr(attributes),
                        TokenType::Lbracket.get_xml_tag(),
                        expression.to_xml(class_symbol_table, local_symbol_table),
                        TokenType::Rbracket.get_xml_tag(),
                    ]),
                    None => token.get_xml_tag_with_attr(attributes),
                }
            }

            Self::IntegerConstant(num) => num.get_xml_tag(),

            Self::StringConstant(s) => s.get_xml_tag(),

            Self::KeywordConstant(keyword) => keyword.0.get_xml_tag(),

            Self::Expresssion(expression) => Element::new_fragment(vec![
                TokenType::Lparen.get_xml_tag(),
                expression.to_xml(class_symbol_table, local_symbol_table),
                TokenType::Rparen.get_xml_tag(),
            ]),

            Self::SubroutineCall(subroutine_call) => {
                subroutine_call.to_xml(class_symbol_table, local_symbol_table)
            }

            Self::UnaryOp(op, term) => Element::new_fragment(vec![
                op.0.get_xml_tag(),
                term.to_xml(class_symbol_table, local_symbol_table),
            ]),
        };

        Element::new_element("term", xml_tag)
    }
}

#[cfg(test)]
mod tests {
    use crate::analyzer::ast::*;
    use crate::analyzer::token::*;
    use crate::analyzer::xml::XmlWriter;
    use std::io::Cursor;

    fn elm_to_str(elm: Element) -> String {
        let mut cursor = Cursor::new(Vec::new());
        cursor.write_xml(&elm).unwrap();
        cursor
            .into_inner()
            .iter()
            .map(|&s| s as char)
            .collect::<String>()
    }

    #[test]
    fn class_to_vm() {
        let actual = Program::new(
            IdentifierToken("Main".to_string()),
            vec![],
            vec![SubroutineDec::new(
                IdentifierToken("Main".to_string()),
                TokenType::Keyword(Keyword::Function),
                None,
                IdentifierToken("main".to_string()),
                vec![],
                vec![],
                vec![
                    Statement::Do(SubroutineCall::new(
                        IdentifierToken("Output".to_string()),
                        IdentifierToken("printInt".to_string()),
                        vec![Expression::new_binary_op(
                            Term::IntegerConstant(1),
                            BinaryOp::new(
                                BinaryOpToken::new(TokenType::Plus),
                                Term::Expresssion(Box::new(Expression::new_binary_op(
                                    Term::IntegerConstant(2),
                                    BinaryOp::new(
                                        BinaryOpToken::new(TokenType::Asterisk),
                                        Term::IntegerConstant(3),
                                    ),
                                ))),
                            ),
                        )],
                    )),
                    Statement::Return(None),
                ],
            )],
        )
        .to_vm();

        assert_eq!(
            VmClass::new(vec![Subroutine::new(
                "Main".to_string(),
                SubroutineType::Function,
                "main".to_string(),
                0,
                vec![
                    Command::Push(Segment::Const, 1),
                    Command::Push(Segment::Const, 2),
                    Command::Push(Segment::Const, 3),
                    Command::Call("Math".to_string(), "multiply".to_string(), 2),
                    Command::Arthmetic(ArthmeticCommand::Add),
                    Command::Call("Output".to_string(), "printInt".to_string(), 1),
                    Command::Pop(Segment::Temp, 0),
                    Command::Push(Segment::Const, 0),
                    Command::Return,
                ]
            )]),
            actual
        );
    }

    #[test]
    fn subroutine_to_vm() {
        let class_symbol_table = SymbolTable::<ClassAttribute>::new();

        let actual = SubroutineDec::new(
            IdentifierToken("Main".to_string()),
            TokenType::Keyword(Keyword::Function),
            None,
            IdentifierToken("main".to_string()),
            vec![],
            vec![],
            vec![
                Statement::Do(SubroutineCall::new(
                    IdentifierToken("Output".to_string()),
                    IdentifierToken("printInt".to_string()),
                    vec![Expression::new_binary_op(
                        Term::IntegerConstant(1),
                        BinaryOp::new(
                            BinaryOpToken::new(TokenType::Plus),
                            Term::Expresssion(Box::new(Expression::new_binary_op(
                                Term::IntegerConstant(2),
                                BinaryOp::new(
                                    BinaryOpToken::new(TokenType::Asterisk),
                                    Term::IntegerConstant(3),
                                ),
                            ))),
                        ),
                    )],
                )),
                Statement::Return(None),
            ],
        )
        .to_vm(&class_symbol_table);

        assert_eq!(
            Subroutine::new(
                "Main".to_string(),
                SubroutineType::Function,
                "main".to_string(),
                0,
                vec![
                    Command::Push(Segment::Const, 1),
                    Command::Push(Segment::Const, 2),
                    Command::Push(Segment::Const, 3),
                    Command::Call("Math".to_string(), "multiply".to_string(), 2),
                    Command::Arthmetic(ArthmeticCommand::Add),
                    Command::Call("Output".to_string(), "printInt".to_string(), 1),
                    Command::Pop(Segment::Temp, 0),
                    Command::Push(Segment::Const, 0),
                    Command::Return,
                ]
            ),
            actual
        );
    }

    #[test]
    fn let_statement_to_vm() {
        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let mut local_symbol_table = SymbolTable::<LocalAttribute>::new();
        local_symbol_table.add_symbol("index".to_string(), "int".to_string(), LocalAttribute::Var);

        let actual = Statement::Let(
            IdentifierToken::new("index"),
            None,
            Expression::new(Term::IntegerConstant(1)),
        )
        .to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(
            vec![
                Command::Push(Segment::Const, 1),
                Command::Pop(Segment::Local, 0),
            ],
            actual
        );
    }

    #[test]
    fn while_statement_to_vm() {
        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        let actual = Statement::While(
            Expression::new_binary_op(
                Term::IntegerConstant(1),
                BinaryOp::new(BinaryOpToken::new(TokenType::Lt), Term::IntegerConstant(2)),
            ),
            "Label".to_string(),
            vec![Statement::Return(Some(Expression::new(
                Term::IntegerConstant(1),
            )))],
        )
        .to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(
            vec![
                Command::Label("Label-while-loop".to_string()),
                Command::Push(Segment::Const, 1),
                Command::Push(Segment::Const, 2),
                Command::Arthmetic(ArthmeticCommand::Lt),
                Command::If("Label-while-statements".to_string()),
                Command::GoTo("Label-while-end".to_string()),
                Command::Label("Label-while-statements".to_string()),
                Command::Push(Segment::Const, 1),
                Command::Return,
                Command::GoTo("Label-while-loop".to_string()),
                Command::Label("Label-while-end".to_string()),
            ],
            actual
        );
    }

    #[test]
    fn if_statement_to_vm() {
        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        let actual = Statement::If(
            Expression::new_binary_op(
                Term::IntegerConstant(1),
                BinaryOp::new(BinaryOpToken::new(TokenType::Lt), Term::IntegerConstant(2)),
            ),
            "Label".to_string(),
            vec![Statement::Return(Some(Expression::new(
                Term::IntegerConstant(1),
            )))],
            None,
        )
        .to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(
            vec![
                Command::Push(Segment::Const, 1),
                Command::Push(Segment::Const, 2),
                Command::Arthmetic(ArthmeticCommand::Lt),
                Command::If("Label-if-statements".to_string()),
                Command::GoTo("Label-if-end".to_string()),
                Command::Label("Label-if-statements".to_string()),
                Command::Push(Segment::Const, 1),
                Command::Return,
                Command::Label("Label-if-end".to_string()),
            ],
            actual
        );
    }

    #[test]
    fn if_else_statement_to_vm() {
        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        let actual = Statement::If(
            Expression::new_binary_op(
                Term::IntegerConstant(1),
                BinaryOp::new(BinaryOpToken::new(TokenType::Lt), Term::IntegerConstant(2)),
            ),
            "Label".to_string(),
            vec![Statement::Return(Some(Expression::new(
                Term::IntegerConstant(1),
            )))],
            Some(vec![Statement::Return(None)]),
        )
        .to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(
            vec![
                Command::Push(Segment::Const, 1),
                Command::Push(Segment::Const, 2),
                Command::Arthmetic(ArthmeticCommand::Lt),
                Command::If("Label-if-statements".to_string()),
                Command::Push(Segment::Const, 0),
                Command::Return,
                Command::GoTo("Label-if-end".to_string()),
                Command::Label("Label-if-statements".to_string()),
                Command::Push(Segment::Const, 1),
                Command::Return,
                Command::Label("Label-if-end".to_string()),
            ],
            actual
        );
    }

    #[test]
    fn do_statement_to_vm() {
        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        let actual = Statement::Do(SubroutineCall::new(
            IdentifierToken("Math".to_string()),
            IdentifierToken("multiply".to_string()),
            vec![
                Expression::new(Term::IntegerConstant(2)),
                Expression::new(Term::IntegerConstant(3)),
            ],
        ))
        .to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(
            vec![
                Command::Push(Segment::Const, 2),
                Command::Push(Segment::Const, 3),
                Command::Call("Math".to_string(), "multiply".to_string(), 2),
                Command::Pop(Segment::Temp, 0)
            ],
            actual
        );
    }

    #[test]
    fn return_to_vm() {
        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        let actual = Statement::Return(Some(Expression::new(Term::IntegerConstant(2))))
            .to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(
            vec![Command::Push(Segment::Const, 2), Command::Return,],
            actual
        );
    }

    #[test]
    fn void_return_to_vm() {
        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        let actual = Statement::Return(None).to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(
            vec![Command::Push(Segment::Const, 0), Command::Return,],
            actual
        );
    }

    #[test]
    fn binary_op_expression_to_vm() {
        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        let actual = Expression::new_binary_op(
            Term::IntegerConstant(2),
            BinaryOp::new(
                BinaryOpToken::new(TokenType::Plus),
                Term::IntegerConstant(3),
            ),
        )
        .to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(
            vec![
                Command::Push(Segment::Const, 2),
                Command::Push(Segment::Const, 3),
                Command::Arthmetic(ArthmeticCommand::Add),
            ],
            actual
        );
    }

    #[test]
    fn expression_term_to_vm() {
        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        let actual = Term::Expresssion(Box::new(Expression::new(Term::IntegerConstant(1))))
            .to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(vec![Command::Push(Segment::Const, 1)], actual);
    }

    #[test]
    fn local_obj_method_call_to_vm() {
        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let mut local_symbol_table = SymbolTable::<LocalAttribute>::new();
        local_symbol_table.add_symbol("math".to_string(), "Math".to_string(), LocalAttribute::Var);

        let actual = Term::SubroutineCall(SubroutineCall::new(
            IdentifierToken("math".to_string()),
            IdentifierToken("multiply".to_string()),
            vec![
                Expression::new(Term::IntegerConstant(2)),
                Expression::new(Term::IntegerConstant(3)),
            ],
        ))
        .to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(
            vec![
                Command::Push(Segment::Local, 0),
                Command::Push(Segment::Const, 2),
                Command::Push(Segment::Const, 3),
                Command::Call("Math".to_string(), "multiply".to_string(), 3),
            ],
            actual
        );
    }

    #[test]
    fn field_obj_method_call_to_vm() {
        let mut class_symbol_table = SymbolTable::<ClassAttribute>::new();
        class_symbol_table.add_symbol(
            "math".to_string(),
            "Math".to_string(),
            ClassAttribute::Field,
        );
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        let actual = Term::SubroutineCall(SubroutineCall::new(
            IdentifierToken("math".to_string()),
            IdentifierToken("multiply".to_string()),
            vec![
                Expression::new(Term::IntegerConstant(2)),
                Expression::new(Term::IntegerConstant(3)),
            ],
        ))
        .to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(
            vec![
                Command::Push(Segment::This, 0),
                Command::Push(Segment::Const, 2),
                Command::Push(Segment::Const, 3),
                Command::Call("Math".to_string(), "multiply".to_string(), 3),
            ],
            actual
        );
    }

    #[test]
    fn self_method_call_to_vm() {
        let mut class_symbol_table = SymbolTable::<ClassAttribute>::new();
        class_symbol_table.add_symbol(
            "Math.multiply".to_string(),
            "subroutine".to_string(),
            ClassAttribute::Method,
        );
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        let actual = Term::SubroutineCall(SubroutineCall::new(
            IdentifierToken("Math".to_string()),
            IdentifierToken("multiply".to_string()),
            vec![
                Expression::new(Term::IntegerConstant(2)),
                Expression::new(Term::IntegerConstant(3)),
            ],
        ))
        .to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(
            vec![
                Command::Push(Segment::Pointer, 0),
                Command::Push(Segment::Const, 2),
                Command::Push(Segment::Const, 3),
                Command::Call("Math".to_string(), "multiply".to_string(), 3),
            ],
            actual
        );
    }

    #[test]
    fn function_call_to_vm() {
        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        let actual = Term::SubroutineCall(SubroutineCall::new(
            IdentifierToken("Math".to_string()),
            IdentifierToken("multiply".to_string()),
            vec![
                Expression::new(Term::IntegerConstant(2)),
                Expression::new(Term::IntegerConstant(3)),
            ],
        ))
        .to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(
            vec![
                Command::Push(Segment::Const, 2),
                Command::Push(Segment::Const, 3),
                Command::Call("Math".to_string(), "multiply".to_string(), 2),
            ],
            actual
        );
    }

    #[test]
    fn integer_constant_to_vm() {
        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        let actual = Term::IntegerConstant(1).to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(vec![Command::Push(Segment::Const, 1)], actual);
    }

    #[test]
    fn keyword_constant_true_to_vm() {
        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        let actual = Term::KeywordConstant(KeywordConstant::new(Keyword::True))
            .to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(
            vec![
                Command::Push(Segment::Const, 1),
                Command::Arthmetic(ArthmeticCommand::Neg)
            ],
            actual
        );
    }

    #[test]
    fn keyword_constant_false_to_vm() {
        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        let actual = Term::KeywordConstant(KeywordConstant::new(Keyword::False))
            .to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(vec![Command::Push(Segment::Const, 0),], actual);
    }

    #[test]
    fn keyword_constant_this_to_vm() {
        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        let actual = Term::KeywordConstant(KeywordConstant::new(Keyword::This))
            .to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(vec![Command::Push(Segment::Pointer, 0),], actual);
    }

    #[test]
    fn var_name_to_vm() {
        let mut class_symbol_table = SymbolTable::<ClassAttribute>::new();
        class_symbol_table.add_symbol(
            "index".to_string(),
            "int".to_string(),
            ClassAttribute::Field,
        );
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        let actual = Term::VarName(IdentifierToken::new("index"), None)
            .to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(vec![Command::Push(Segment::This, 0)], actual);
    }

    #[test]
    fn unary_op_to_vm() {
        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        let actual = Term::UnaryOp(
            UnaryOpToken::new(TokenType::Minus),
            Box::new(Term::IntegerConstant(1)),
        )
        .to_vm(&class_symbol_table, &local_symbol_table);

        assert_eq!(
            vec![
                Command::Push(Segment::Const, 1),
                Command::Arthmetic(ArthmeticCommand::Neg)
            ],
            actual
        );
    }

    #[test]
    fn program_to_xml() {
        let program = Program::new(
            IdentifierToken::new("Square"),
            vec![ClassVarDec::new(
                TokenType::Keyword(Keyword::Field),
                ClassTypeToken::new(TokenType::Keyword(Keyword::Int)),
                IdentifierToken::new("size"),
                vec![],
            )],
            vec![SubroutineDec::new(
                IdentifierToken::new("Square"),
                TokenType::Keyword(Keyword::Method),
                None,
                IdentifierToken::new("main"),
                vec![],
                vec![
                    VarDec::new(
                        ClassTypeToken::new(TokenType::Identifier(IdentifierToken::new(
                            "SquareGame",
                        ))),
                        IdentifierToken::new("game"),
                        vec![],
                    ),
                    VarDec::new(
                        ClassTypeToken::new(TokenType::Keyword(Keyword::Int)),
                        IdentifierToken::new("num"),
                        vec![],
                    ),
                ],
                vec![
                    Statement::Let(
                        IdentifierToken::new("game"),
                        None,
                        Expression::new(Term::VarName(IdentifierToken::new("game"), None)),
                    ),
                    Statement::Do(SubroutineCall::new(
                        IdentifierToken::new("game"),
                        IdentifierToken::new("run"),
                        vec![],
                    )),
                    Statement::Return(None),
                ],
            )],
        );

        assert_eq!(
            elm_to_str(program.to_xml()),
            "<class>
  <keyword> class </keyword>
  <identifier> Square </identifier>
  <symbol> { </symbol>
  <classVarDec>
    <keyword> field </keyword>
    <keyword> int </keyword>
    <identifier category=\"int\" how=\"define\" attribute=\"field\"> size </identifier>
    <symbol> ; </symbol>
  </classVarDec>
  <subroutineDec>
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
        <identifier category=\"SquareGame\" how=\"define\" attribute=\"var\"> game </identifier>
        <symbol> ; </symbol>
      </varDec>
      <varDec>
        <keyword> var </keyword>
        <keyword> int </keyword>
        <identifier category=\"int\" how=\"define\" attribute=\"var\"> num </identifier>
        <symbol> ; </symbol>
      </varDec>
      <statements>
        <letStatement>
          <keyword> let </keyword>
          <identifier> game </identifier>
          <symbol> = </symbol>
          <expression>
            <term>
              <identifier category=\"SquareGame\" how=\"use\" attribute=\"var\"> game </identifier>
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
  </subroutineDec>
  <symbol> } </symbol>
</class>
"
        );
    }

    #[test]
    fn class_var_dec_to_xml() {
        let class_var_dec = ClassVarDec::new(
            TokenType::Keyword(Keyword::Static),
            ClassTypeToken::new(TokenType::Keyword(Keyword::Boolean)),
            IdentifierToken::new("test"),
            vec![],
        );

        assert_eq!(
            elm_to_str(class_var_dec.to_xml()),
            "<classVarDec>
  <keyword> static </keyword>
  <keyword> boolean </keyword>
  <identifier category=\"boolean\" how=\"define\" attribute=\"static\"> test </identifier>
  <symbol> ; </symbol>
</classVarDec>
"
        );
    }

    #[test]
    fn class_var_multiple_dec_to_xml() {
        let class_var_dec = ClassVarDec::new(
            TokenType::Keyword(Keyword::Field),
            ClassTypeToken::new(TokenType::Keyword(Keyword::Int)),
            IdentifierToken::new("i"),
            vec![IdentifierToken::new("j1"), IdentifierToken::new("j2")],
        );

        assert_eq!(
            elm_to_str(class_var_dec.to_xml()),
            "<classVarDec>
  <keyword> field </keyword>
  <keyword> int </keyword>
  <identifier category=\"int\" how=\"define\" attribute=\"field\"> i </identifier>
  <symbol> , </symbol>
  <identifier category=\"int\" how=\"define\" attribute=\"field\"> j1 </identifier>
  <symbol> , </symbol>
  <identifier category=\"int\" how=\"define\" attribute=\"field\"> j2 </identifier>
  <symbol> ; </symbol>
</classVarDec>
"
        );
    }

    #[test]
    fn var_dec_to_xml() {
        let var_dec = VarDec::new(
            ClassTypeToken::new(TokenType::Identifier(IdentifierToken::new("String"))),
            IdentifierToken::new("s"),
            vec![],
        );

        assert_eq!(
            elm_to_str(var_dec.to_xml()),
            "<varDec>
  <keyword> var </keyword>
  <identifier> String </identifier>
  <identifier category=\"String\" how=\"define\" attribute=\"var\"> s </identifier>
  <symbol> ; </symbol>
</varDec>
"
        );
    }

    #[test]
    fn no_parameter_subroutine_dec_to_xml() {
        let subroutine_dec = SubroutineDec::new(
            IdentifierToken::new("Square"),
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
                Statement::Do(SubroutineCall::new(
                    IdentifierToken::new("game"),
                    IdentifierToken::new("run"),
                    vec![],
                )),
                Statement::Return(None),
            ],
        );

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();

        assert_eq!(
            elm_to_str(subroutine_dec.to_xml(&class_symbol_table)),
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
      <identifier category=\"SquareGame\" how=\"define\" attribute=\"var\"> game </identifier>
      <symbol> ; </symbol>
    </varDec>
    <statements>
      <letStatement>
        <keyword> let </keyword>
        <identifier> game </identifier>
        <symbol> = </symbol>
        <expression>
          <term>
            <identifier category=\"SquareGame\" how=\"use\" attribute=\"var\"> game </identifier>
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
</subroutineDec>
"
        );
    }

    #[test]
    fn parameter_subroutine_dec_to_xml() {
        let subroutine_dec = SubroutineDec::new(
            IdentifierToken::new("Square"),
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
                Statement::Do(SubroutineCall::new(
                    IdentifierToken::new("game"),
                    IdentifierToken::new("run"),
                    vec![],
                )),
                Statement::Return(None),
            ],
        );

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();

        assert_eq!(
            elm_to_str(subroutine_dec.to_xml(&class_symbol_table)),
            "<subroutineDec>
  <keyword> method </keyword>
  <keyword> boolean </keyword>
  <identifier> main </identifier>
  <symbol> ( </symbol>
  <parameterList>
    <keyword> boolean </keyword>
    <identifier category=\"boolean\" how=\"define\" attribute=\"argument\"> b </identifier>
    <symbol> , </symbol>
    <keyword> char </keyword>
    <identifier category=\"char\" how=\"define\" attribute=\"argument\"> c </identifier>
  </parameterList>
  <symbol> ) </symbol>
  <subroutineBody>
    <symbol> { </symbol>
    <varDec>
      <keyword> var </keyword>
      <identifier> SquareGame </identifier>
      <identifier category=\"SquareGame\" how=\"define\" attribute=\"var\"> game </identifier>
      <symbol> ; </symbol>
    </varDec>
    <statements>
      <letStatement>
        <keyword> let </keyword>
        <identifier> game </identifier>
        <symbol> = </symbol>
        <expression>
          <term>
            <identifier category=\"SquareGame\" how=\"use\" attribute=\"var\"> game </identifier>
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
</subroutineDec>
"
        );
    }

    #[test]
    fn var_multiple_dec_to_xml() {
        let var_dec = VarDec::new(
            ClassTypeToken::new(TokenType::Keyword(Keyword::Int)),
            IdentifierToken::new("i"),
            vec![IdentifierToken::new("j")],
        );

        assert_eq!(
            elm_to_str(var_dec.to_xml()),
            "<varDec>
  <keyword> var </keyword>
  <keyword> int </keyword>
  <identifier category=\"int\" how=\"define\" attribute=\"var\"> i </identifier>
  <symbol> , </symbol>
  <identifier category=\"int\" how=\"define\" attribute=\"var\"> j </identifier>
  <symbol> ; </symbol>
</varDec>
"
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

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let mut local_symbol_table = SymbolTable::<LocalAttribute>::new();
        local_symbol_table.add_symbol("i".to_string(), "int".to_string(), LocalAttribute::Argument);

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
            "<expression>
  <term>
    <identifier category=\"int\" how=\"use\" attribute=\"argument\"> i </identifier>
  </term>
  <symbol> + </symbol>
  <term>
    <integerConstant> 2 </integerConstant>
  </term>
</expression>
"
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

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let mut local_symbol_table = SymbolTable::<LocalAttribute>::new();
        local_symbol_table.add_symbol(
            "anotherVar".to_string(),
            "char".to_string(),
            LocalAttribute::Argument,
        );

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
            "<letStatement>
  <keyword> let </keyword>
  <identifier> myVar </identifier>
  <symbol> = </symbol>
  <expression>
    <term>
      <identifier category=\"char\" how=\"use\" attribute=\"argument\"> anotherVar </identifier>
    </term>
  </expression>
  <symbol> ; </symbol>
</letStatement>
"
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

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let mut local_symbol_table = SymbolTable::<LocalAttribute>::new();
        local_symbol_table.add_symbol("i".to_string(), "int".to_string(), LocalAttribute::Var);
        local_symbol_table.add_symbol(
            "anotherVar".to_string(),
            "bool".to_string(),
            LocalAttribute::Argument,
        );

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
            "<letStatement>
  <keyword> let </keyword>
  <identifier> myVar </identifier>
  <symbol> [ </symbol>
  <expression>
    <term>
      <identifier category=\"int\" how=\"use\" attribute=\"var\"> i </identifier>
    </term>
  </expression>
  <symbol> ] </symbol>
  <symbol> = </symbol>
  <expression>
    <term>
      <identifier category=\"bool\" how=\"use\" attribute=\"argument\"> anotherVar </identifier>
    </term>
  </expression>
  <symbol> ; </symbol>
</letStatement>
"
        )
    }

    #[test]
    fn while_statement_to_xml() {
        let program = Statement::While(
            Expression::new(Term::KeywordConstant(KeywordConstant::new(Keyword::True))),
            "Label".to_string(),
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

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
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
</whileStatement>
"
        )
    }

    #[test]
    fn if_statement_no_else_to_xml() {
        let program = Statement::If(
            Expression::new(Term::KeywordConstant(KeywordConstant::new(Keyword::True))),
            "Label".to_string(),
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

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
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
</ifStatement>
"
        )
    }

    #[test]
    fn if_statement_has_else_to_xml() {
        let program = Statement::If(
            Expression::new(Term::KeywordConstant(KeywordConstant::new(Keyword::True))),
            "Label".to_string(),
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

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
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
</ifStatement>
"
        )
    }

    #[test]
    fn do_statement_to_xml() {
        let program = Statement::Do(SubroutineCall::new(
            IdentifierToken::new("game"),
            IdentifierToken::new("run"),
            vec![],
        ));

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
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
</doStatement>
"
        )
    }

    #[test]
    fn return_statement_no_identifier_to_xml() {
        let program = Statement::Return(None);

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
            "<returnStatement>
  <keyword> return </keyword>
  <symbol> ; </symbol>
</returnStatement>
"
        )
    }

    #[test]
    fn return_statement_that_has_identifier_to_xml() {
        let program = Statement::Return(Some(Expression {
            left_term: Term::VarName(IdentifierToken::new("square"), None),
            binary_op: None,
        }));

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let mut local_symbol_table = SymbolTable::<LocalAttribute>::new();
        local_symbol_table.add_symbol(
            "square".to_string(),
            "bool".to_string(),
            LocalAttribute::Argument,
        );

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
            "<returnStatement>
  <keyword> return </keyword>
  <expression>
    <term>
      <identifier category=\"bool\" how=\"use\" attribute=\"argument\"> square </identifier>
    </term>
  </expression>
  <symbol> ; </symbol>
</returnStatement>
"
        )
    }

    #[test]
    fn var_name_expression_to_xml() {
        let program = Statement::Expression(Expression {
            left_term: Term::VarName(IdentifierToken::new("foo"), None),
            binary_op: None,
        });

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let mut local_symbol_table = SymbolTable::<LocalAttribute>::new();
        local_symbol_table.add_symbol(
            "foo".to_string(),
            "bool".to_string(),
            LocalAttribute::Argument,
        );

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
            "<expression>
  <term>
    <identifier category=\"bool\" how=\"use\" attribute=\"argument\"> foo </identifier>
  </term>
</expression>
"
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

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let mut local_symbol_table = SymbolTable::<LocalAttribute>::new();
        local_symbol_table.add_symbol(
            "foo".to_string(),
            "bool".to_string(),
            LocalAttribute::Argument,
        );

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
            "<expression>
  <term>
    <identifier category=\"bool\" how=\"use\" attribute=\"argument\"> foo </identifier>
    <symbol> [ </symbol>
    <expression>
      <term>
        <integerConstant> 1 </integerConstant>
      </term>
    </expression>
    <symbol> ] </symbol>
  </term>
</expression>
"
        )
    }

    #[test]
    fn string_constant_expression_to_xml() {
        let program = Statement::Expression(Expression {
            left_term: Term::StringConstant("str value!!".to_string()),
            binary_op: None,
        });

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
            "<expression>
  <term>
    <stringConstant> str value!! </stringConstant>
  </term>
</expression>
"
        )
    }

    #[test]
    fn keyword_constant_expression_to_xml() {
        let program = Statement::Expression(Expression::new(Term::KeywordConstant(
            KeywordConstant::new(Keyword::True),
        )));

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
            "<expression>
  <term>
    <keyword> true </keyword>
  </term>
</expression>
"
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

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let mut local_symbol_table = SymbolTable::<LocalAttribute>::new();
        local_symbol_table.add_symbol("i".to_string(), "int".to_string(), LocalAttribute::Argument);

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
            "<expression>
  <term>
    <symbol> ( </symbol>
    <expression>
      <term>
        <symbol> - </symbol>
        <term>
          <identifier category=\"int\" how=\"use\" attribute=\"argument\"> i </identifier>
        </term>
      </term>
    </expression>
    <symbol> ) </symbol>
  </term>
</expression>
"
        )
    }

    #[test]
    fn unary_op_expression_to_xml() {
        let program = Statement::Expression(Expression::new(Term::UnaryOp(
            UnaryOpToken::new(TokenType::Minus),
            Box::new(Term::IntegerConstant(1)),
        )));

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
            "<expression>
  <term>
    <symbol> - </symbol>
    <term>
      <integerConstant> 1 </integerConstant>
    </term>
  </term>
</expression>
"
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
        let elm = Element::new_fragment(
            program
                .iter()
                .map(|s| {
                    s.to_xml(
                        &SymbolTable::<ClassAttribute>::new(),
                        &SymbolTable::<LocalAttribute>::new(),
                    )
                })
                .collect::<Vec<_>>(),
        );
        let mut cursor = Cursor::new(Vec::new());
        cursor.write_xml(&elm).unwrap();

        let actual = cursor
            .into_inner()
            .iter()
            .map(|&s| s as char)
            .collect::<String>();

        assert_eq!(
            actual,
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
</expression>
"
        )
    }

    #[test]
    fn local_subroutine_call_no_param_expression_to_xml() {
        let program =
            Statement::Expression(Expression::new(Term::SubroutineCall(SubroutineCall::new(
                IdentifierToken::new("Main"),
                IdentifierToken::new("new"),
                vec![],
            ))));

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
            "<expression>
  <term>
    <identifier> Main </identifier>
    <symbol> . </symbol>
    <identifier> new </identifier>
    <symbol> ( </symbol>
    <expressionList>
    </expressionList>
    <symbol> ) </symbol>
  </term>
</expression>
"
        )
    }

    #[test]
    fn local_subroutine_call_with_param_expression_to_xml() {
        let program =
            Statement::Expression(Expression::new(Term::SubroutineCall(SubroutineCall::new(
                IdentifierToken::new("Main"),
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

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
            "<expression>
  <term>
    <identifier> Main </identifier>
    <symbol> . </symbol>
    <identifier> new </identifier>
    <symbol> ( </symbol>
    <expressionList>
      <expression>
        <term>
          <integerConstant> 1 </integerConstant>
        </term>
      </expression>
      <symbol> , </symbol>
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
</expression>
"
        )
    }

    #[test]
    fn subroutine_call_no_param_expression_to_xml() {
        let program =
            Statement::Expression(Expression::new(Term::SubroutineCall(SubroutineCall::new(
                IdentifierToken::new("SquareGame"),
                IdentifierToken::new("new"),
                vec![],
            ))));

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
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
</expression>
"
        )
    }

    #[test]
    fn subroutine_call_with_param_expression_to_xml() {
        let program =
            Statement::Expression(Expression::new(Term::SubroutineCall(SubroutineCall::new(
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
            ))));

        let class_symbol_table = SymbolTable::<ClassAttribute>::new();
        let local_symbol_table = SymbolTable::<LocalAttribute>::new();

        assert_eq!(
            elm_to_str(program.to_xml(&class_symbol_table, &local_symbol_table)),
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
      <symbol> , </symbol>
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
</expression>
"
        )
    }
}
