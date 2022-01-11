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
            Keyword::CLASS => "<keyword> class </keyword>",
            Keyword::CONSTRUCTOR => "<keyword> constructor </keyword>",
            Keyword::FUNCTION => "<keyword> function </keyword>",
            Keyword::METHOD => "<keyword> method </keyword>",
            Keyword::FIELD => "<keyword> field </keyword>",
            Keyword::STATIC => "<keyword> static </keyword>",
            Keyword::VAR => "<keyword> var </keyword>",
            Keyword::INT => "<keyword> int </keyword>",
            Keyword::CHAR => "<keyword> char </keyword>",
            Keyword::BOOLEAN => "<keyword> boolean </keyword>",
            Keyword::VOID => "<keyword> void </keyword>",
            Keyword::TRUE => "<keyword> true </keyword>",
            Keyword::FALSE => "<keyword> false </keyword>",
            Keyword::NULL => "<keyword> null </keyword>",
            Keyword::THIS => "<keyword> this </keyword>",
            Keyword::LET => "<keyword> let </keyword>",
            Keyword::DO => "<keyword> do </keyword>",
            Keyword::IF => "<keyword> if </keyword>",
            Keyword::ELSE => "<keyword> else </keyword>",
            Keyword::WHILE => "<keyword> while </keyword>",
            Keyword::RETURN => "<keyword> return </keyword>",
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
            TokenType::LBRACE => "<symbol> { </symbol>".to_string(),
            TokenType::RBRACE => "<symbol> } </symbol>".to_string(),
            TokenType::LPAREN => "<symbol> ( </symbol>".to_string(),
            TokenType::RPAREN => "<symbol> ) </symbol>".to_string(),
            TokenType::LBRACKET => "<symbol> [ </symbol>".to_string(),
            TokenType::RBRACKET => "<symbol> ] </symbol>".to_string(),
            TokenType::DOT => "<symbol> . </symbol>".to_string(),
            TokenType::COMMA => "<symbol> , </symbol>".to_string(),
            TokenType::SEMICOLON => "<symbol> ; </symbol>".to_string(),
            TokenType::PLUS => "<symbol> + </symbol>".to_string(),
            TokenType::MINUS => "<symbol> - </symbol>".to_string(),
            TokenType::ASTERISK => "<symbol> * </symbol>".to_string(),
            TokenType::SLASH => "<symbol> / </symbol>".to_string(),
            TokenType::AND => "<symbol> &amp; </symbol>".to_string(),
            TokenType::OR => "<symbol> | </symbol>".to_string(),
            TokenType::LT => "<symbol> &lt; </symbol>".to_string(),
            TokenType::GT => "<symbol> &gt; </symbol>".to_string(),
            TokenType::ASSIGN => "<symbol> = </symbol>".to_string(),
            TokenType::TILDE => "<symbol> ~ </symbol>".to_string(),
            TokenType::KEYWORD(keyword) => keyword.get_literal().to_string(),
            TokenType::IDNETIFIER(ident) => format!("<identifier> {} </identifier>", ident),
            TokenType::NUMBER(num) => format!("<integerConstant> {} </integerConstant>", num),
            TokenType::STRING(str) => format!("<stringConstant> {} </stringConstant>", str),
            _ => "".to_string(),
        }
    }
}
