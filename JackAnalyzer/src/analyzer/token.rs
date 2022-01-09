#[derive(PartialEq, Debug, Clone)]
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
    pub fn get_literal(&self) -> &str {
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

#[derive(PartialEq, Debug, Clone)]
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

    pub fn get_literal(&self) -> String {
        match self {
            TokenType::LBRACE => "{".to_string(),
            TokenType::RBRACE => "}".to_string(),
            TokenType::LPAREN => "(".to_string(),
            TokenType::RPAREN => ")".to_string(),
            TokenType::LBRACKET => "[".to_string(),
            TokenType::RBRACKET => "]".to_string(),
            TokenType::DOT => ".".to_string(),
            TokenType::COMMA => ",".to_string(),
            TokenType::SEMICOLON => ";".to_string(),
            TokenType::PLUS => "+".to_string(),
            TokenType::MINUS => "-".to_string(),
            TokenType::ASTERISK => "*".to_string(),
            TokenType::SLASH => "/".to_string(),
            TokenType::AND => "&amp;".to_string(),
            TokenType::OR => "|".to_string(),
            TokenType::LT => "&lt;".to_string(),
            TokenType::GT => "&gt;".to_string(),
            TokenType::ASSIGN => "=".to_string(),
            TokenType::TILDE => "~".to_string(),
            TokenType::KEYWORD(keyword) => keyword.get_literal().to_string(),
            TokenType::IDNETIFIER(ident) => ident.to_string(),
            TokenType::NUMBER(num) => num.to_string(),
            TokenType::STRING(str) => str.to_string(),
            _ => "".to_string(),
        }
    }
}
