use std::collections::HashMap;
use std::fmt;
use std::fmt::Formatter;

#[derive(fmt::Debug, Clone, Copy)]
pub enum TokenType {
    Nil,       // nil
    Object,    // object
    Bind,      // :=
    Arrow,     // <-
    Dot,       // .
    Comma,     // ,
    Name,      // xyz you know
    String,    // ""
    Atom,      // :atom
    Lambda,    // @
    Delimiter, // |
    Return,    // ^
    Num,       // 123 you know
    LParen,    // ( or [
    RParen,    // ) or ]
    Eof,
    // Comment    NB
}

impl TokenType {
    pub fn is_gen_sym(self) -> bool {
        use TokenType::*;
        matches!(
            self,
            Nil | Object | Name | Atom | Lambda | Num | String | LParen
        )
    }
}

pub fn make_keywords() -> HashMap<&'static str, TokenType> {
    let mut keywords = HashMap::new();
    keywords.insert("nil", TokenType::Nil);
    keywords.insert("object", TokenType::Object);
    keywords.insert("<-", TokenType::Arrow);
    keywords
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub(crate) token: TokenType,
    pub(crate) content: String,
    pub(crate) span: (i32, i32),
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
}
