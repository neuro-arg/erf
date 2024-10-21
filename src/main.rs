#![allow(clippy::match_single_binding)]
#![allow(clippy::overly_complex_bool_expr)]
#![allow(dead_code)]

use ast::{Expr, ExprInner, Ident, LiteralKind, Tld};

mod ast;
mod diag;
mod hir;
mod hir_eval;
mod mir;
// mod cst;
// mod lex;
mod grammar;
mod typeck;
mod util;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Span {
    file: u8,
    left: usize,
    right: usize,
}

impl Span {
    pub fn new(file: u8, left: usize, right: usize) -> Self {
        Self { file, left, right }
    }
    pub fn text<'a>(&self, src: &'a str) -> &'a str {
        &src[self.left..self.right]
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct ParseCtx {
    file: u8,
}

// construct using context methods instead of directly for easier refactoring
impl ParseCtx {
    pub fn span(&mut self, l: usize, r: usize) -> Span {
        (l, r).construct(self)
    }
    pub fn expr_literal(
        &mut self,
        literal: LiteralKind,
        content: impl Construct<Ident>,
        span: impl Construct<Span>,
    ) -> Expr {
        Expr::new(
            ExprInner::literal(literal, content.construct(self)),
            span.construct(self),
        )
    }
    pub fn expr_var(&mut self, content: impl Construct<Ident>, span: impl Construct<Span>) -> Expr {
        Expr::new(
            ExprInner::var(content.construct(self)),
            span.construct(self),
        )
    }
    pub fn let_pattern_var(
        &mut self,
        content: impl Construct<Ident>,
        span: impl Construct<Span>,
    ) -> ast::LetPattern {
        ast::LetPattern::new(content.construct(self), span.construct(self))
    }
    pub fn pattern_var(
        &mut self,
        content: impl Construct<Ident>,
        span: impl Construct<Span>,
    ) -> ast::Pattern {
        ast::Pattern::new(
            ast::PatternInner::Variable(content.construct(self)),
            span.construct(self),
        )
    }
    pub fn unary(
        &mut self,
        op: ast::UnOp,
        expr: impl Construct<Box<ast::Expr>>,
        span: impl Construct<Span>,
    ) -> Expr {
        Expr::new(
            ExprInner::unary(op, expr.construct(self)),
            span.construct(self),
        )
    }
    pub fn binary(
        &mut self,
        op: ast::BinOp,
        expr1: impl Construct<Box<ast::Expr>>,
        expr2: impl Construct<Box<ast::Expr>>,
        span: impl Construct<Span>,
    ) -> Expr {
        Expr::new(
            ExprInner::binary(op, expr1.construct(self), expr2.construct(self)),
            span.construct(self),
        )
    }
    pub fn lambda(
        &mut self,
        pattern: impl Construct<ast::Pattern>,
        expr: impl Construct<Box<ast::Expr>>,
        span: impl Construct<Span>,
    ) -> Expr {
        Expr::new(
            ExprInner::lambda(pattern.construct(self), expr.construct(self)),
            span.construct(self),
        )
    }
    pub fn let_arm(
        &mut self,
        pat: impl Construct<ast::LetPattern>,
        expr: impl Construct<Expr>,
    ) -> ast::LetArm {
        (pat.construct(self), expr.construct(self))
    }
    pub fn letrec(
        &mut self,
        arms: impl Construct<Vec<ast::LetArm>>,
        body: impl Construct<Box<ast::Expr>>,
        span: impl Construct<Span>,
    ) -> Expr {
        Expr::new(
            ExprInner::letrec(arms.construct(self), body.construct(self)),
            span.construct(self),
        )
    }
    pub fn tld(
        &mut self,
        pattern: impl Construct<ast::LetPattern>,
        expr1: impl Construct<ast::Expr>,
        span: impl Construct<Span>,
    ) -> Tld {
        Tld::new(
            pattern.construct(self),
            expr1.construct(self),
            span.construct(self),
        )
    }
}

pub trait Construct<T> {
    fn construct(self, ctx: &mut ParseCtx) -> T;
}

impl<T> Construct<T> for T {
    fn construct(self, _ctx: &mut ParseCtx) -> T {
        self
    }
}

impl<T> Construct<Box<T>> for T {
    fn construct(self, _ctx: &mut ParseCtx) -> Box<T> {
        Box::new(self)
    }
}
impl Construct<Span> for (usize, usize) {
    fn construct(self, ctx: &mut ParseCtx) -> Span {
        Span {
            file: ctx.file,
            left: self.0,
            right: self.1,
        }
    }
}

fn show_err(filename: &str, code: &str, err: diag::Error) {
    let cache = (filename, ariadne::Source::from(code));
    err.to_ariadne(
        &[filename],
        ariadne::Report::build(ariadne::ReportKind::Error, filename, 0),
    )
    .eprint(cache)
    .unwrap();
}

fn main() {
    let parser = grammar::ScriptParser::default();
    let input_file = std::env::args().nth(1).expect("expected filename");
    let text = std::fs::read_to_string(&input_file).expect("unable to read file");
    println!("text -> ast");
    let ast = match parser
        .parse(&mut ParseCtx { file: 0 }, &text)
        .map_err(|err| {
            diag::Error::Lalrpop(
                0,
                err.map_token(|lalrpop_util::lexer::Token(n, s)| (n, s.to_owned()))
                    .map_error(|s| s.to_owned()),
            )
        }) {
        Ok(x) => x,
        Err(err) => {
            show_err(&input_file, &text, err);
            std::process::exit(1);
        }
    };
    let mut ctx = hir::Ctx::default();
    println!("ast -> hir");
    let (bindings, scope) = match ctx.lower_ast(ast) {
        Ok(x) => x,
        Err(err) => {
            show_err(&input_file, &text, err);
            std::process::exit(1);
        }
    };
    // println!("{:#?}", scope);
    let graph = ctx.ck.graphviz(&text);
    std::fs::write("types.dot", graph).unwrap();
    let handle = std::process::Command::new("dot")
        .args(["types.dot", "-Tpng", "-o", "types.png"])
        .spawn();
    let main_var = bindings.get("main").expect("main not found");
    let (main_var, poly) = match main_var {
        hir::VarType::Mono(var) => (var, false),
        hir::VarType::Poly(var) => (var, true),
    };
    let ty = ctx.ck.add_pos(typeck::Pos::Var(main_var), Span::default());
    let ty = if poly { ctx.ck.monomorphize(ty, 0) } else { ty };
    println!("hir -> value");
    println!(
        "{:?}",
        hir_eval::eval_term(
            &mut scope.try_into().unwrap(),
            hir::Term {
                inner: hir::TermInner::VarAccess(main_var),
                ty,
            }
        )
        .unwrap()
    );
    let _ = handle.ok().and_then(|mut handle| handle.wait().ok());
}
