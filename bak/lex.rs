// this is just a backup, i'll use lalrpop for now
use std::{fmt, ops::Range};

use logos::Logos;

#[derive(Copy, Clone, Debug, Logos, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum Token {
    #[token("false")]
    #[token("true")]
    Bool,

    #[regex(r"[\s]+")]
    Whitespace,

    #[regex(r"#.*")]
    Comment,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("let")]
    Let,

    #[token("if")]
    If,

    #[token("then")]
    Then,

    #[token("else")]
    Else,

    #[token(";")]
    Semicolon,

    #[token("=")]
    Assign,

    #[token("+")]
    Add,

    #[token("-")]
    Sub,

    #[token("*")]
    Mul,

    #[token("//")]
    FloorDiv,

    #[token("%")]
    Rem,

    #[token("/")]
    Div,

    #[token("==")]
    Eq,

    #[token("!=")]
    Ne,

    #[token("!")]
    Not,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token("&")]
    BitAnd,

    #[token("|")]
    BitOr,

    #[token("<<")]
    ShiftLeft,

    #[token(">>")]
    ShiftRight,

    #[token("^")]
    BitXor,

    #[token("~")]
    BitInv,

    #[token("->")]
    Func,

    #[regex(r"(?:\p{XID_Start}|_)\p{XID_Continue}*")]
    Ident,

    #[regex(r"-?(?:0|[1-9]\d*)")]
    Int,

    #[regex(r"-?(?:0|[1-9]\d*)\.\d+(?:[eE][+-]?\d+)?")]
    Float,

    #[regex(r#""(?:[^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#)]
    String,

    // must be last
    Error,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub struct Lexer<'a>(logos::Lexer<'a, Token>);

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Self {
        Self(Token::lexer(s))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Token, &'a str, Range<usize>);
    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .next()
            .map(|x| (x.unwrap_or(Token::Error), self.0.slice(), self.0.span()))
    }
}

#[cfg(test)]
mod tests {
    use crate::lex::{Lexer, Token};

    #[test]
    fn test() {
        let s = r#"
            x = let a = 5; let b = 6.0; if a == 5 then b else c; # test
            y = x -> y -> x;
            w = 0 -> x
              | 1 -> y
              | 2 -> z;
            # other test
        "#;
        assert_eq!(
            &Lexer::new(s)
                .map(|(a, b, _)| (a, b))
                .filter(|x| !matches!(x.0, Token::Whitespace))
                .collect::<Vec<_>>(),
            &[
                (Token::Ident, "x"),
                (Token::Assign, "="),
                (Token::Let, "let"),
                (Token::Ident, "a"),
                (Token::Assign, "="),
                (Token::Int, "5"),
                (Token::Semicolon, ";"),
                (Token::Let, "let"),
                (Token::Ident, "b"),
                (Token::Assign, "="),
                (Token::Float, "6.0"),
                (Token::Semicolon, ";"),
                (Token::If, "if"),
                (Token::Ident, "a"),
                (Token::Eq, "=="),
                (Token::Int, "5"),
                (Token::Then, "then"),
                (Token::Ident, "b"),
                (Token::Else, "else"),
                (Token::Ident, "c"),
                (Token::Semicolon, ";"),
                (Token::Comment, "# test"),
                (Token::Comment, "# other test"),
            ]
        )
    }
}
