#[derive(PartialEq, Debug)]
pub enum Keyword {
    CLASS,
    CONSTRUCTOR,
    FUNCTION,
    METHOD,
    FIELD,
    STATIC,
    VAR,
    INT,
    CHAR,
    BOOLEAN,
    VOID,
    TRUE,
    FALSE,
    NULL,
    THIS,
    LET,
    DO,
    IF,
    ELSE,
    WHILE,
    RETURN,
}

impl Keyword {
    pub fn to_string(&self) -> &str {
        match self {
            Keyword::CLASS => "class",
            Keyword::CONSTRUCTOR => "constructor",
            Keyword::FUNCTION => "function",
            Keyword::METHOD => "method",
            Keyword::FIELD => "field",
            Keyword::STATIC => "static",
            Keyword::VAR => "var",
            Keyword::INT => "int",
            Keyword::CHAR => "char",
            Keyword::BOOLEAN => "boolean",
            Keyword::VOID => "void",
            Keyword::TRUE => "true",
            Keyword::FALSE => "false",
            Keyword::NULL => "null",
            Keyword::THIS => "this",
            Keyword::LET => "let",
            Keyword::DO => "do",
            Keyword::IF => "if",
            Keyword::ELSE => "else",
            Keyword::WHILE => "while",
            Keyword::RETURN => "return",
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum TokenType {
    LBRACE,    // "{"
    RBRACE,    // "}"
    LPAREN,    // "("
    RPAREN,    // ")"
    LBRACKET,  // "["
    RBRACKET,  // "]"
    DOT,       //.
    COMMA,     // ","
    SEMICOLON, // ";"
    PLUS,      //  "+"
    MINUS,     // "-"
    ASTERISK,  // "*"
    SLASH,     // "/"
    AND,       // &
    OR,        // |
    LT,        // "<"
    GT,        // ">"
    ASSIGN,    // "="
    TILDE,     // ~
    IDNETIFIER(String),
    NUMBER(i32),
    STRING(String),
    KEYWORD(Keyword),
    COMMENTS,
    EOF,
}

impl TokenType {
    pub fn lookup_identify(identify: &str) -> TokenType {
        match identify {
            "class" => TokenType::KEYWORD(Keyword::CLASS),
            "constructor" => TokenType::KEYWORD(Keyword::CONSTRUCTOR),
            "function" => TokenType::KEYWORD(Keyword::FUNCTION),
            "method" => TokenType::KEYWORD(Keyword::METHOD),
            "field" => TokenType::KEYWORD(Keyword::FIELD),
            "static" => TokenType::KEYWORD(Keyword::STATIC),
            "var" => TokenType::KEYWORD(Keyword::VAR),
            "int" => TokenType::KEYWORD(Keyword::INT),
            "char" => TokenType::KEYWORD(Keyword::CHAR),
            "boolean" => TokenType::KEYWORD(Keyword::BOOLEAN),
            "void" => TokenType::KEYWORD(Keyword::VOID),
            "true" => TokenType::KEYWORD(Keyword::TRUE),
            "false" => TokenType::KEYWORD(Keyword::FALSE),
            "null" => TokenType::KEYWORD(Keyword::NULL),
            "this" => TokenType::KEYWORD(Keyword::THIS),
            "let" => TokenType::KEYWORD(Keyword::LET),
            "do" => TokenType::KEYWORD(Keyword::DO),
            "if" => TokenType::KEYWORD(Keyword::IF),
            "else" => TokenType::KEYWORD(Keyword::ELSE),
            "while" => TokenType::KEYWORD(Keyword::WHILE),
            "return" => TokenType::KEYWORD(Keyword::RETURN),
            _ => TokenType::IDNETIFIER(identify.to_string()),
        }
    }

    pub fn to_string(&self) -> &str {
        match self {
            TokenType::LBRACE => "{",
            TokenType::RBRACE => "}",
            TokenType::LPAREN => "(",
            TokenType::RPAREN => ")",
            TokenType::LBRACKET => "[",
            TokenType::RBRACKET => "]",
            TokenType::DOT => ".",
            TokenType::COMMA => ",",
            TokenType::SEMICOLON => ";",
            TokenType::PLUS => "+",
            TokenType::MINUS => "-",
            TokenType::ASTERISK => "*",
            TokenType::SLASH => "/",
            TokenType::AND => "&amp;",
            TokenType::OR => "|",
            TokenType::LT => "&lt;",
            TokenType::GT => "&gt;",
            TokenType::ASSIGN => "=",
            TokenType::TILDE => "~",
            TokenType::KEYWORD(keyword) => keyword.to_string(),
            _ => "",
        }
    }
}
