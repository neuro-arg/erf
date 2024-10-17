// this is just a backup, i'll use lalrpop for now
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}

macro_rules! syntax_kind {
    {$($i:tt: $kind:tt,)*} => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[allow(non_camel_case_types)]
        #[repr(u16)]
        pub enum SyntaxKind {
            Token(Token),
            // composite
            $($kind,)*
        }
        const LAST_SYNTAX_KIND: u16 = {
            $(
                #[allow(unused)]
                let x = $i;
            )*
            x
        };
        impl From<Token> for rowan::SyntaxKind {
            fn from(tok: Token) -> Self {
                Self(tok as u16 + LAST_SYNTAX_KIND + 1)
            }
        }
        impl From<SyntaxKind> for rowan::SyntaxKind {
            fn from(kind: SyntaxKind) -> Self {
                match kind {
                    $(SyntaxKind::$kind => Self($i),)*
                    SyntaxKind::Token(x) => Self::from(x),
                }
            }
        }
        impl rowan::Language for Lang {
            type Kind = SyntaxKind;
            fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
                match raw.0 {
                    $($i => SyntaxKind::$kind,)*
                    x => {
                        assert!(x > LAST_SYNTAX_KIND && x - LAST_SYNTAX_KIND - 1 <= Token::Error as u16);
                        Self::Kind::Token(unsafe { std::mem::transmute::<u16, Token>(x - LAST_SYNTAX_KIND - 1) })
                    }
                }
            }
            fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
                kind.into()
            }
        }
    };
}

syntax_kind! {
    0: List,
    1: Atom,
    2: Root,
    3: TopLevelDecl,
    4: Pattern,
    5: UnOp,
    6: BinOp,
    7: Literal,
}

impl From<Token> for SyntaxKind {
    fn from(value: Token) -> Self {
        Self::Token(value)
    }
}

use std::ops::Range;

/// GreenNode is an immutable tree, which is cheap to change,
/// but doesn't contain offsets and parent pointers.
use rowan::GreenNode;

/// You can construct GreenNodes by hand, but a builder
/// is helpful for top-down parsers: it maintains a stack
/// of currently in-progress nodes
use rowan::GreenNodeBuilder;
use thiserror::Error;

use crate::lex::Lexer;
use crate::lex::Token;

#[derive(Debug, Error)]
enum ParseError {
    #[error("unclosed parenthesis")]
    UnclosedParen(Range<usize>),
    #[error("unexpected token")]
    Unexpected(Token, Range<usize>),
    #[error("unexpected end of file")]
    UnexpectedEof,
}

#[derive(Copy, Clone)]
#[repr(u16)]
enum OpPrecedence {
    Unary,
    MulDiv,
    AddSub,
    BitShift,
    BitAnd,
    BitXor,
    BitOr,
    Cmp,
    And,
    Or,
}

impl OpPrecedence {
    fn next(self) -> Option<Self> {
        match self {
            Self::Or => None,
            _ => Some(unsafe { std::mem::transmute::<u16, Self>(self as u16 + 1) }),
        }
    }
    fn prev(self) -> Option<Self> {
        match self as u16 {
            0 => None,
            x => Some(unsafe { std::mem::transmute::<u16, Self>(x - 1) }),
        }
    }
}

#[derive(Debug)]
pub struct Parse {
    pub green_node: GreenNode,
    #[allow(unused)]
    errors: Vec<ParseError>,
}

struct Parser<'a> {
    lexer: std::iter::Peekable<Lexer<'a>>,
    builder: GreenNodeBuilder<'a>,
    errors: Vec<ParseError>,
}

enum Res {
    Ok,
    Eof,
    End,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer: lexer.peekable(),
            builder: GreenNodeBuilder::new(),
            errors: vec![],
        }
    }
    fn finish(self) -> Parse {
        Parse {
            green_node: self.builder.finish(),
            errors: self.errors,
        }
    }
    fn collect(&mut self, mut next: impl FnMut(&mut Self) -> Res, eof_ok: bool) {
        loop {
            match next(self) {
                Res::Eof if eof_ok => break,
                Res::End => break,
                Res::Eof => {
                    self.node(Token::Error, |this| {
                        this.errors.push(ParseError::UnexpectedEof);
                    });
                    break;
                }
                Res::Ok => {}
            }
        }
    }
    fn node(&mut self, node: impl Into<SyntaxKind>, mut build: impl FnMut(&mut Self)) {
        self.builder.start_node(node.into().into());
        self.skip_ws();
        build(self);
        self.builder.finish_node();
    }
    fn root(&mut self) {
        self.node(SyntaxKind::Root, |this| {
            this.collect(Self::sexp, true);
            this.skip_ws();
            while this.current().is_some() {
                this.unexpected(true);
            }
        });
    }
    fn inner_decl(&mut self) {
        loop {
            self.skip_ws();
            match self.current() {
                Some(Token::Assign) => break,
                None => return,
                _ => {}
            }
            self.pattern();
        }
        self.expect(Token::Assign);
        self.expr();
        self.skip_ws();
        self.expect(Token::Semicolon);
    }
    fn expr_level(&mut self, level: OpPrecedence) {
        self.skip_ws();
        match level {
            OpPrecedence::Unary => match self.current() {
                None => {
                    self.unexpected(false);
                    return;
                }
                Some(Token::Whitespace) => unreachable!(),
                Some(Token::Semicolon) => return,
                Some(Token::BitInv | Token::Not) => {
                    self.node(SyntaxKind::UnOp, |this| {
                        this.bump();
                        this.expr_level(level);
                    });
                }
                Some(Token::Bool | Token::Int | Token::Float) => {
                    self.node(SyntaxKind::Literal, |this| {
                        this.bump();
                    });
                }
            },
        }
        self.node(SyntaxKind::Expr, |this| {
            this.expect(Token::Ident);
        });
    }
    fn expr(&mut self) {
        self.node(SyntaxKind::Expr, |this| {
            this.expect(Token::Ident);
        });
    }
    fn pattern(&mut self) {
        self.node(SyntaxKind::Pattern, |this| {
            this.expect(Token::Ident);
        });
    }
    fn top_level(&mut self) {
        self.node(SyntaxKind::TopLevelDecl, |this| {
            this.inner_decl();
        });
    }
    fn list(&mut self) {
        assert_eq!(self.current(), Some(Token::LParen));
        self.node(SyntaxKind::List, |this| {
            this.bump();
            if let Some(Token::RParen) = this.current() {
                this.bump();
            } else {
                this.collect(
                    |this| match Self::sexp(this) {
                        Res::Ok => {
                            if let Some(Token::RParen) = this.current() {
                                this.bump();
                                return Res::End;
                            }
                            this.skip_ws();
                            Res::Ok
                        }
                        x => x,
                    },
                    false,
                );
            }
        });
    }
    fn sexp(&mut self) -> Res {
        self.skip_ws();
        let Some(t) = self.current2() else {
            return Res::Eof;
        };
        match t.0 {
            Token::LParen => self.list(),
            Token::Ident | Token::Int | Token::Add | Token::Div | Token::Mul => {
                self.node(SyntaxKind::Atom, |this| {
                    this.bump();
                });
            }
            Token::Error => self.bump(),
            _ => {
                self.unexpected(true);
            }
        }
        Res::Ok
    }
    fn expect(&mut self, tok: Token) {
        match self.current2() {
            Some((t, _)) if t == tok => self.bump(),
            _ => self.unexpected(false),
        }
    }
    fn unexpected(&mut self, bump: bool) {
        self.node(Token::Error, |this| {
            if let Some((tok, span)) = this.current2() {
                this.errors.push(ParseError::Unexpected(tok, span));
                if bump {
                    this.bump();
                }
            } else {
                this.errors.push(ParseError::UnexpectedEof);
            }
        });
    }
    fn bump(&mut self) {
        let (kind, text, _span) = self.lexer.next().unwrap();
        self.builder.token(kind.into(), text);
    }
    fn current(&mut self) -> Option<Token> {
        self.current2().map(|(a, _)| a)
    }
    fn current2(&mut self) -> Option<(Token, Range<usize>)> {
        self.lexer
            .peek()
            .map(|(kind, _, span)| (*kind, span.clone()))
    }
    fn skip_ws(&mut self) {
        while self.current() == Some(Token::Whitespace) {
            self.bump()
        }
    }
}

pub fn parse(text: &str) -> Parse {
    let tokens = crate::lex::Lexer::new(text);
    let mut parser = Parser::new(tokens);
    parser.root();
    parser.finish()
}

type SyntaxNode = rowan::SyntaxNode<Lang>;
#[allow(unused)]
type SyntaxToken = rowan::SyntaxToken<Lang>;
#[allow(unused)]
type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

impl Parse {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }
}
