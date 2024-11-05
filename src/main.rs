#![allow(clippy::match_single_binding)]
#![allow(clippy::overly_complex_bool_expr)]
#![allow(clippy::type_complexity)]
#![allow(dead_code)]

use std::collections::BTreeMap;

use ast::{Expr, ExprInner, Ident, LetArm, LiteralKind};

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
    pub fn let_pattern_type(
        &mut self,
        ident: impl Construct<Ident>,
        span: impl Construct<Span>,
    ) -> (ast::LetPattern, Expr) {
        let ident = ident.construct(self);
        let span = span.construct(self);
        let pat = ast::LetPattern::new_val(ident.clone(), span);
        let expr = Expr::new(ExprInner::TypeConstructor(ident), span);
        (pat, expr)
    }
    pub fn let_pattern_var(
        &mut self,
        ident: impl Construct<Ident>,
        span: impl Construct<Span>,
    ) -> ast::LetPattern {
        ast::LetPattern::new_val(ident.construct(self), span.construct(self))
    }
    pub fn let_pattern_func(
        &mut self,
        content: impl Construct<Ident>,
        args: impl Construct<Vec<ast::Pattern>>,
        span: impl Construct<Span>,
    ) -> ast::LetPattern {
        ast::LetPattern::new_func(
            content.construct(self),
            args.construct(self),
            span.construct(self),
        )
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
    pub fn pattern_tagged(
        &mut self,
        tags: impl Construct<Vec<(Ident, Span)>>,
        pat: impl Construct<ast::Pattern>,
        _span: impl Construct<Span>,
    ) -> ast::Pattern {
        let tags = tags.construct(self);
        let pat = pat.construct(self);
        tags.into_iter().rev().fold(pat, |pat, (tag, span)| {
            let mut span1 = pat.span;
            span1.left = span.left;
            ast::Pattern::new(ast::PatternInner::tag(tag, span, pat), span1)
        })
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
        span: impl Construct<Span>,
    ) -> ast::LetArm {
        ast::LetArm::new(
            pat.construct(self),
            expr.construct(self),
            span.construct(self),
        )
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
    pub fn tld(&mut self, arm: impl Construct<ast::LetArm>) -> LetArm {
        arm.construct(self)
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

struct Wrapper<T>(T);

impl<A, B, C: Construct<A>, D: Construct<B>> Construct<Vec<(A, B)>> for Wrapper<Vec<(C, D)>> {
    fn construct(self, ctx: &mut ParseCtx) -> Vec<(A, B)> {
        self.0
            .into_iter()
            .map(|(a, b)| (a.construct(ctx), b.construct(ctx)))
            .collect()
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
    let mut ast1 = BTreeMap::<_, Vec<_>>::new();
    for x in ast {
        ast1.entry(x.pattern.ident.clone()).or_default().push(x);
    }
    let ast = ast1;
    let mut ctx = hir::Ctx::default();
    println!("ast -> hir");
    let (mut bindings, scope) = match ctx.lower_ast(ast) {
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
    let (main_var, ty, _meta) = match bindings
        .get("main", Span::default(), &mut ctx.ck, 0)
        .transpose()
        .ok_or_else(|| {
            diag::Error::NameNotFound(diag::NameNotFoundError::new("main", Span::default(), false))
        })
        .and_then(|x| x)
    {
        Ok(x) => x,
        Err(err) => {
            show_err(&input_file, &text, err);
            std::process::exit(1);
        }
    };
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
