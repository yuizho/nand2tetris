#[derive(PartialEq, Debug, Clone)]
pub enum Keyword {
    Class,
    Constructor,
    Function,
    Method,
    Field,
    Static,
    Var,
    Int,
    Char,
    Boolean,
    Void,
    True,
    False,
    Null,
    This,
    Let,
    Do,
    If,
    Else,
    While,
    Return,
}

pub trait Token {
    fn get_xml_tag(&self) -> String;
}

impl Token for Keyword {
    fn get_xml_tag(&self) -> String {
        let xml_tag = match self {
            Keyword::Class => "<keyword> class </keyword>",
            Keyword::Constructor => "<keyword> constructor </keyword>",
            Keyword::Function => "<keyword> function </keyword>",
            Keyword::Method => "<keyword> method </keyword>",
            Keyword::Field => "<keyword> field </keyword>",
            Keyword::Static => "<keyword> static </keyword>",
            Keyword::Var => "<keyword> var </keyword>",
            Keyword::Int => "<keyword> int </keyword>",
            Keyword::Char => "<keyword> char </keyword>",
            Keyword::Boolean => "<keyword> boolean </keyword>",
            Keyword::Void => "<keyword> void </keyword>",
            Keyword::True => "<keyword> true </keyword>",
            Keyword::False => "<keyword> false </keyword>",
            Keyword::Null => "<keyword> null </keyword>",
            Keyword::This => "<keyword> this </keyword>",
            Keyword::Let => "<keyword> let </keyword>",
            Keyword::Do => "<keyword> do </keyword>",
            Keyword::If => "<keyword> if </keyword>",
            Keyword::Else => "<keyword> else </keyword>",
            Keyword::While => "<keyword> while </keyword>",
            Keyword::Return => "<keyword> return </keyword>",
        };
        xml_tag.to_string()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct IdentifierToken {
    identifier: String,
}
impl IdentifierToken {
    pub fn new<S: Into<String>>(identifier: S) -> Self {
        IdentifierToken {
            identifier: identifier.into(),
        }
    }
}
impl Token for IdentifierToken {
    fn get_xml_tag(&self) -> String {
        format!("<identifier> {} </identifier>", self.identifier)
    }
}

impl Token for i32 {
    fn get_xml_tag(&self) -> String {
        format!("<integerConstant> {} </integerConstant>", self)
    }
}

impl Token for String {
    fn get_xml_tag(&self) -> String {
        format!("<stringConstant> {} </stringConstant>", self)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum TokenType {
    Lbrace,    // "{"
    Rbrace,    // "}"
    Lparen,    // "("
    Rparen,    // ")"
    Lbracket,  // "["
    Rbracket,  // "]"
    Dot,       //.
    Comma,     // ","
    Semicolon, // ";"
    Plus,      //  "+"
    Minus,     // "-"
    Asterisk,  // "*"
    Slash,     // "/"
    And,       // &
    Or,        // |
    Lt,        // "<"
    Gt,        // ">"
    Assign,    // "="
    Tilde,     // ~
    Identifier(IdentifierToken),
    Number(i32),
    String(String),
    Keyword(Keyword),
    Comments,
    Eof,
}

impl TokenType {
    pub fn lookup_identify(identify: &str) -> TokenType {
        match identify {
            "class" => TokenType::Keyword(Keyword::Class),
            "constructor" => TokenType::Keyword(Keyword::Constructor),
            "function" => TokenType::Keyword(Keyword::Function),
            "method" => TokenType::Keyword(Keyword::Method),
            "field" => TokenType::Keyword(Keyword::Field),
            "static" => TokenType::Keyword(Keyword::Static),
            "var" => TokenType::Keyword(Keyword::Var),
            "int" => TokenType::Keyword(Keyword::Int),
            "char" => TokenType::Keyword(Keyword::Char),
            "boolean" => TokenType::Keyword(Keyword::Boolean),
            "void" => TokenType::Keyword(Keyword::Void),
            "true" => TokenType::Keyword(Keyword::True),
            "false" => TokenType::Keyword(Keyword::False),
            "null" => TokenType::Keyword(Keyword::Null),
            "this" => TokenType::Keyword(Keyword::This),
            "let" => TokenType::Keyword(Keyword::Let),
            "do" => TokenType::Keyword(Keyword::Do),
            "if" => TokenType::Keyword(Keyword::If),
            "else" => TokenType::Keyword(Keyword::Else),
            "while" => TokenType::Keyword(Keyword::While),
            "return" => TokenType::Keyword(Keyword::Return),
            _ => TokenType::Identifier(IdentifierToken {
                identifier: identify.to_string(),
            }),
        }
    }

    pub fn get_xml_tag(&self) -> String {
        match self {
            TokenType::Lbrace => "<symbol> { </symbol>".to_string(),
            TokenType::Rbrace => "<symbol> } </symbol>".to_string(),
            TokenType::Lparen => "<symbol> ( </symbol>".to_string(),
            TokenType::Rparen => "<symbol> ) </symbol>".to_string(),
            TokenType::Lbracket => "<symbol> [ </symbol>".to_string(),
            TokenType::Rbracket => "<symbol> ] </symbol>".to_string(),
            TokenType::Dot => "<symbol> . </symbol>".to_string(),
            TokenType::Comma => "<symbol> , </symbol>".to_string(),
            TokenType::Semicolon => "<symbol> ; </symbol>".to_string(),
            TokenType::Plus => "<symbol> + </symbol>".to_string(),
            TokenType::Minus => "<symbol> - </symbol>".to_string(),
            TokenType::Asterisk => "<symbol> * </symbol>".to_string(),
            TokenType::Slash => "<symbol> / </symbol>".to_string(),
            TokenType::And => "<symbol> &amp; </symbol>".to_string(),
            TokenType::Or => "<symbol> | </symbol>".to_string(),
            TokenType::Lt => "<symbol> &lt; </symbol>".to_string(),
            TokenType::Gt => "<symbol> &gt; </symbol>".to_string(),
            TokenType::Assign => "<symbol> = </symbol>".to_string(),
            TokenType::Tilde => "<symbol> ~ </symbol>".to_string(),
            TokenType::Keyword(keyword) => keyword.get_xml_tag(),
            TokenType::Identifier(ident) => ident.get_xml_tag(),
            TokenType::Number(num) => num.get_xml_tag(),
            TokenType::String(str) => format!("<stringConstant> {} </stringConstant>", str),
            _ => "".to_string(),
        }
    }
}
