use super::xml::Element;

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
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct IdentifierToken(String);
impl IdentifierToken {
    pub fn new<S: Into<String>>(identifier: S) -> Self {
        IdentifierToken(identifier.into())
    }
}
impl Token for IdentifierToken {
    fn get_xml_tag(&self) -> Element {
        Element::new_text("identifier", &self.0)
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

    pub fn is(&self, token: TokenType) -> bool {
        self == &token
    }

    pub fn is_eof_or(&self, token: TokenType) -> bool {
        self == &token || self == &TokenType::Eof
    }
}
