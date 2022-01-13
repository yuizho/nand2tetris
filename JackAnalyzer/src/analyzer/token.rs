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

pub trait Token {
    fn get_xml_tag(&self) -> String;
}

impl Token for Keyword {
    fn get_xml_tag(&self) -> String {
        match self {
            Keyword::CLASS => "<keyword> class </keyword>".to_string(),
            Keyword::CONSTRUCTOR => "<keyword> constructor </keyword>".to_string(),
            Keyword::FUNCTION => "<keyword> function </keyword>".to_string(),
            Keyword::METHOD => "<keyword> method </keyword>".to_string(),
            Keyword::FIELD => "<keyword> field </keyword>".to_string(),
            Keyword::STATIC => "<keyword> static </keyword>".to_string(),
            Keyword::VAR => "<keyword> var </keyword>".to_string(),
            Keyword::INT => "<keyword> int </keyword>".to_string(),
            Keyword::CHAR => "<keyword> char </keyword>".to_string(),
            Keyword::BOOLEAN => "<keyword> boolean </keyword>".to_string(),
            Keyword::VOID => "<keyword> void </keyword>".to_string(),
            Keyword::TRUE => "<keyword> true </keyword>".to_string(),
            Keyword::FALSE => "<keyword> false </keyword>".to_string(),
            Keyword::NULL => "<keyword> null </keyword>".to_string(),
            Keyword::THIS => "<keyword> this </keyword>".to_string(),
            Keyword::LET => "<keyword> let </keyword>".to_string(),
            Keyword::DO => "<keyword> do </keyword>".to_string(),
            Keyword::IF => "<keyword> if </keyword>".to_string(),
            Keyword::ELSE => "<keyword> else </keyword>".to_string(),
            Keyword::WHILE => "<keyword> while </keyword>".to_string(),
            Keyword::RETURN => "<keyword> return </keyword>".to_string(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct IdentifierToken {
    pub identifier: String,
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
    IDNETIFIER(IdentifierToken),
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
            _ => TokenType::IDNETIFIER(IdentifierToken {
                identifier: identify.to_string(),
            }),
        }
    }

    pub fn get_xml_tag(&self) -> String {
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
            TokenType::KEYWORD(keyword) => keyword.get_xml_tag(),
            TokenType::IDNETIFIER(ident) => ident.get_xml_tag(),
            TokenType::NUMBER(num) => num.get_xml_tag(),
            TokenType::STRING(str) => format!("<stringConstant> {} </stringConstant>", str),
            _ => "".to_string(),
        }
    }
}
