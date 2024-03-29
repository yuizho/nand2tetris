use super::xml::{Attribute, Element};

#[derive(PartialEq, Eq, Debug, Clone)]
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
    fn get_xml_tag_with_attr(&self, attributers: Vec<Attribute>) -> Element;
}

impl Token for Keyword {
    fn get_xml_tag(&self) -> Element {
        use Keyword::*;
        match self {
            Class => Element::new_text("keyword", "class"),
            Constructor => Element::new_text("keyword", "constructor"),
            Function => Element::new_text("keyword", "function"),
            Method => Element::new_text("keyword", "method"),
            Field => Element::new_text("keyword", "field"),
            Static => Element::new_text("keyword", "static"),
            Var => Element::new_text("keyword", "var"),
            Int => Element::new_text("keyword", "int"),
            Char => Element::new_text("keyword", "char"),
            Boolean => Element::new_text("keyword", "boolean"),
            Void => Element::new_text("keyword", "void"),
            True => Element::new_text("keyword", "true"),
            False => Element::new_text("keyword", "false"),
            Null => Element::new_text("keyword", "null"),
            This => Element::new_text("keyword", "this"),
            Let => Element::new_text("keyword", "let"),
            Do => Element::new_text("keyword", "do"),
            If => Element::new_text("keyword", "if"),
            Else => Element::new_text("keyword", "else"),
            While => Element::new_text("keyword", "while"),
            Return => Element::new_text("keyword", "return"),
        }
    }

    fn get_xml_tag_with_attr(&self, attributers: Vec<Attribute>) -> Element {
        use Keyword::*;
        match self {
            Class => Element::new_text_with_attr("keyword", "class", attributers),
            Constructor => Element::new_text_with_attr("keyword", "constructor", attributers),
            Function => Element::new_text_with_attr("keyword", "function", attributers),
            Method => Element::new_text_with_attr("keyword", "method", attributers),
            Field => Element::new_text_with_attr("keyword", "field", attributers),
            Static => Element::new_text_with_attr("keyword", "static", attributers),
            Var => Element::new_text_with_attr("keyword", "var", attributers),
            Int => Element::new_text_with_attr("keyword", "int", attributers),
            Char => Element::new_text_with_attr("keyword", "char", attributers),
            Boolean => Element::new_text_with_attr("keyword", "boolean", attributers),
            Void => Element::new_text_with_attr("keyword", "void", attributers),
            True => Element::new_text_with_attr("keyword", "true", attributers),
            False => Element::new_text_with_attr("keyword", "false", attributers),
            Null => Element::new_text_with_attr("keyword", "null", attributers),
            This => Element::new_text_with_attr("keyword", "this", attributers),
            Let => Element::new_text_with_attr("keyword", "let", attributers),
            Do => Element::new_text_with_attr("keyword", "do", attributers),
            If => Element::new_text_with_attr("keyword", "if", attributers),
            Else => Element::new_text_with_attr("keyword", "else", attributers),
            While => Element::new_text_with_attr("keyword", "while", attributers),
            Return => Element::new_text_with_attr("keyword", "return", attributers),
        }
    }
}
impl ToString for Keyword {
    fn to_string(&self) -> String {
        use Keyword::*;
        let s = match self {
            Class => "class",
            Constructor => "constructor",
            Function => "function",
            Method => "method",
            Field => "field",
            Static => "static",
            Var => "var",
            Int => "int",
            Char => "char",
            Boolean => "boolean",
            Void => "void",
            True => "true",
            False => "false",
            Null => "null",
            This => "this",
            Let => "let",
            Do => "do",
            If => "if",
            Else => "else",
            While => "while",
            Return => "return",
        };
        s.to_string()
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct IdentifierToken(pub String);
impl IdentifierToken {
    pub fn new<S: Into<String>>(identifier: S) -> Self {
        IdentifierToken(identifier.into())
    }
}
impl Token for IdentifierToken {
    fn get_xml_tag(&self) -> Element {
        Element::new_text("identifier", &self.0)
    }
    fn get_xml_tag_with_attr(&self, attributes: Vec<Attribute>) -> Element {
        Element::new_text_with_attr("identifier", &self.0, attributes)
    }
}

impl Token for i32 {
    fn get_xml_tag(&self) -> Element {
        Element::new_text("integerConstant", &self.to_string())
    }
    fn get_xml_tag_with_attr(&self, attributes: Vec<Attribute>) -> Element {
        Element::new_text_with_attr("integerConstant", &self.to_string(), attributes)
    }
}

impl Token for String {
    fn get_xml_tag(&self) -> Element {
        Element::new_text("stringConstant", self)
    }
    fn get_xml_tag_with_attr(&self, attributes: Vec<Attribute>) -> Element {
        Element::new_text_with_attr("stringConstant", self, attributes)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
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
            _ => TokenType::Identifier(IdentifierToken(identify.to_string())),
        }
    }

    pub fn get_xml_tag(&self) -> Element {
        use TokenType::*;
        match self {
            Lbrace => Element::new_text("symbol", "{"),
            Rbrace => Element::new_text("symbol", "}"),
            Lparen => Element::new_text("symbol", "("),
            Rparen => Element::new_text("symbol", ")"),
            Lbracket => Element::new_text("symbol", "["),
            Rbracket => Element::new_text("symbol", "]"),
            Dot => Element::new_text("symbol", "."),
            Comma => Element::new_text("symbol", ","),
            Semicolon => Element::new_text("symbol", ";"),
            Plus => Element::new_text("symbol", "+"),
            Minus => Element::new_text("symbol", "-"),
            Asterisk => Element::new_text("symbol", "*"),
            Slash => Element::new_text("symbol", "/"),
            And => Element::new_text("symbol", "&amp;"),
            Or => Element::new_text("symbol", "|"),
            Lt => Element::new_text("symbol", "&lt;"),
            Gt => Element::new_text("symbol", "&gt;"),
            Assign => Element::new_text("symbol", "="),
            Tilde => Element::new_text("symbol", "~"),
            Keyword(keyword) => keyword.get_xml_tag(),
            Identifier(ident) => ident.get_xml_tag(),
            Number(num) => num.get_xml_tag(),
            String(str) => Element::new_text("stringConstant", str),
            _ => Element::empty(),
        }
    }

    pub fn is(&self, token: &TokenType) -> bool {
        self == token
    }

    pub fn is_eof_or(&self, token: &TokenType) -> bool {
        self == token || self == &TokenType::Eof
    }
}
