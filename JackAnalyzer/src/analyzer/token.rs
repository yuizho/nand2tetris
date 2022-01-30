use super::xml::Element;

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
    fn get_xml_tag(&self) -> Element;
}

impl Token for Keyword {
    fn get_xml_tag(&self) -> Element {
        match self {
            Keyword::Class => Element::new_text("keyword", "class"),
            Keyword::Constructor => Element::new_text("keyword", "constructor"),
            Keyword::Function => Element::new_text("keyword", "function"),
            Keyword::Method => Element::new_text("keyword", "method"),
            Keyword::Field => Element::new_text("keyword", "field"),
            Keyword::Static => Element::new_text("keyword", "static"),
            Keyword::Var => Element::new_text("keyword", "var"),
            Keyword::Int => Element::new_text("keyword", "int"),
            Keyword::Char => Element::new_text("keyword", "char"),
            Keyword::Boolean => Element::new_text("keyword", "boolean"),
            Keyword::Void => Element::new_text("keyword", "void"),
            Keyword::True => Element::new_text("keyword", "true"),
            Keyword::False => Element::new_text("keyword", "false"),
            Keyword::Null => Element::new_text("keyword", "null"),
            Keyword::This => Element::new_text("keyword", "this"),
            Keyword::Let => Element::new_text("keyword", "let"),
            Keyword::Do => Element::new_text("keyword", "do"),
            Keyword::If => Element::new_text("keyword", "if"),
            Keyword::Else => Element::new_text("keyword", "else"),
            Keyword::While => Element::new_text("keyword", "while"),
            Keyword::Return => Element::new_text("keyword", "return"),
        }
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
    fn get_xml_tag(&self) -> Element {
        Element::new_text("identifier", &self.identifier)
    }
}

impl Token for i32 {
    fn get_xml_tag(&self) -> Element {
        Element::new_text("integerConstant", &self.to_string())
    }
}

impl Token for String {
    fn get_xml_tag(&self) -> Element {
        Element::new_text("stringConstant", self)
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

    pub fn get_xml_tag(&self) -> Element {
        match self {
            TokenType::Lbrace => Element::new_text("symbol", "{"),
            TokenType::Rbrace => Element::new_text("symbol", "}"),
            TokenType::Lparen => Element::new_text("symbol", "("),
            TokenType::Rparen => Element::new_text("symbol", ")"),
            TokenType::Lbracket => Element::new_text("symbol", "["),
            TokenType::Rbracket => Element::new_text("symbol", "]"),
            TokenType::Dot => Element::new_text("symbol", "."),
            TokenType::Comma => Element::new_text("symbol", ","),
            TokenType::Semicolon => Element::new_text("symbol", ";"),
            TokenType::Plus => Element::new_text("symbol", "+"),
            TokenType::Minus => Element::new_text("symbol", "-"),
            TokenType::Asterisk => Element::new_text("symbol", "*"),
            TokenType::Slash => Element::new_text("symbol", "/"),
            TokenType::And => Element::new_text("symbol", "&amp;"),
            TokenType::Or => Element::new_text("symbol", "|"),
            TokenType::Lt => Element::new_text("symbol", "&lt;"),
            TokenType::Gt => Element::new_text("symbol", "&gt;"),
            TokenType::Assign => Element::new_text("symbol", "="),
            TokenType::Tilde => Element::new_text("symbol", "~"),
            TokenType::Keyword(keyword) => keyword.get_xml_tag(),
            TokenType::Identifier(ident) => ident.get_xml_tag(),
            TokenType::Number(num) => num.get_xml_tag(),
            TokenType::String(str) => Element::new_text("stringConstant", str),
            _ => Element::empty(),
        }
    }

    pub fn is(&self, token: TokenType) -> bool {
        self == &token
    }

    pub fn is_eof_or(&self, token: TokenType) -> bool {
        self == &token || self == &TokenType::Eof
    }
}
