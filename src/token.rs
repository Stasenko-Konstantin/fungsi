use std::collections::HashMap;
use std::fmt;
use std::fmt::Formatter;

#[derive(fmt::Debug)]
pub enum TokenType {
    DEF,       // def
    NIL,       // nil
    OBJECT,    // object
    BIND,      // :=
    EQ,        // =
    COMMA,     // ,
    NAME,
    ATOM,      // :atom
    LAMBDA,    // @
    DELIMITER, // |
    RETURN,    // ^
    NUM,
    LPAREN,    // ( or [
    RPAREN,    // ) or ]
    EOF,
}

pub fn make_keywords() -> HashMap<&'static str, TokenType> {
    let mut keywords = HashMap::new();
    keywords.insert("def", TokenType::DEF);
    keywords.insert("nil", TokenType::NIL);
    keywords.insert("object", TokenType::OBJECT);
    keywords.insert(":=", TokenType::BIND);
    keywords.insert("=", TokenType::EQ);
    keywords.insert(",", TokenType::COMMA);
    keywords.insert(":", TokenType::ATOM);
    keywords.insert("@", TokenType::LAMBDA);
    keywords.insert("|", TokenType::DELIMITER);
    keywords.insert("^", TokenType::RETURN);
    keywords.insert("(", TokenType::LPAREN);
    keywords.insert(")", TokenType::RPAREN);
    keywords
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug)]
pub struct Token {
    pub(crate) token: TokenType,
    pub(crate) content: String,
    pub(crate) span: (i64, i64),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{{ {} , {} }}", self.token, self.content)
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.content == other.content
    }
    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}