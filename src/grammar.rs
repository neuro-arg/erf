// auto-generated: "lalrpop 0.20.2"
// sha3: 874e996f39266ed5fa2c1132287daa78a4ef3e66da30139d92ca62b9e4cdd566
use super::ast::{self, Expr, Pattern};
use super::Span;
#[allow(unused_extern_crates)]
extern crate lalrpop_util as __lalrpop_util;
#[allow(unused_imports)]
use self::__lalrpop_util::state_machine as __state_machine;
extern crate core;
extern crate alloc;

#[rustfmt::skip]
#[allow(non_snake_case, non_camel_case_types, unused_mut, unused_variables, unused_imports, unused_parens, clippy::needless_lifetimes, clippy::type_complexity, clippy::needless_return, clippy::too_many_arguments, clippy::never_loop, clippy::match_single_binding, clippy::needless_raw_string_hashes)]
mod __parse__Script {

    use super::super::ast::{self, Expr, Pattern};
    use super::super::Span;
    #[allow(unused_extern_crates)]
    extern crate lalrpop_util as __lalrpop_util;
    #[allow(unused_imports)]
    use self::__lalrpop_util::state_machine as __state_machine;
    extern crate core;
    extern crate alloc;
    use self::__lalrpop_util::lexer::Token;
    #[allow(dead_code)]
    pub(crate) enum __Symbol<'input>
     {
        Variant0(&'input str),
        Variant1(usize),
        Variant2(Expr),
        Variant3(String),
        Variant4(Pattern),
        Variant5(alloc::vec::Vec<ast::Tld>),
        Variant6((Expr, Expr, Span)),
        Variant7((String, Span)),
        Variant8(ast::Tld),
    }
    const __ACTION: &[i8] = &[
        // State 0
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 0, 0, 0,
        // State 1
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 0, 0, 0,
        // State 2
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 36, 61, 62, 63,
        // State 3
        0, -23, 0, -23, -23, -23, 6, -23, -23, -23, -23, -23, 0, -23, -23, -23, -23, -23, -23, 0, -23, -23, -23, -23, 0, -23, 0, -23, -23, 36, 61, 62, 63,
        // State 4
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 5
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 36, 61, 62, 63,
        // State 6
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 0, 0, 0,
        // State 7
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 8
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 9
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 10
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 11
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 12
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 13
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 14
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 15
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 16
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 17
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 18
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 19
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 20
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 21
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 22
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 23
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 24
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 25
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 26
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 27
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 28
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 61, 62, 63,
        // State 29
        5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 36, 61, 62, 63,
        // State 30
        0, -52, 0, -52, -52, -52, -52, -52, -52, -52, -52, -52, -52, -52, -52, -52, -52, -52, -52, -52, -52, -52, -52, -52, 0, -52, 0, -52, -52, -52, -52, -52, -52,
        // State 31
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 32
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 33
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -47, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 34
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -59, 0, 0, 0,
        // State 35
        0, -44, 0, -44, -44, -44, -44, -44, -44, -44, -44, -44, -44, -44, -44, -44, -44, -44, -44, -44, -44, -44, -44, -44, 0, -44, 0, -44, -44, -44, -44, -44, -44,
        // State 36
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -60, 0, 0, 0,
        // State 37
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 38
        0, -8, 0, -8, -8, -8, -8, -8, -8, -8, -8, -8, 0, -8, -8, -8, -8, -8, -8, 0, -8, -8, -8, -8, 0, -8, 0, -8, -8, -8, -8, -8, -8,
        // State 39
        0, 8, 0, 0, 0, -17, 0, -17, 0, 0, 0, 0, 0, 0, 0, -17, 9, 0, 10, 0, 11, 12, 13, 0, 0, 0, 0, 0, -17, 0, 0, 0, 0,
        // State 40
        0, 0, 0, 0, 0, 14, 0, -19, 0, 0, 0, 0, 0, 0, 0, -19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -19, 0, 0, 0, 0,
        // State 41
        0, 0, 0, 0, 0, 0, 0, -21, 0, 0, 0, 0, 0, 0, 0, -21, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15, 0, 0, 0, 0,
        // State 42
        0, 0, 0, 0, 0, 0, 0, -5, 0, 0, 0, 0, 0, 0, 0, -5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 43
        0, -25, 0, -25, -25, -25, 0, -25, -25, 16, -25, -25, 0, -25, -25, -25, -25, -25, -25, 0, -25, -25, -25, -25, 0, -25, 0, -25, -25, 0, 0, 0, 0,
        // State 44
        0, -30, 0, -30, -30, -30, 0, -30, -30, 0, -30, -30, 0, -30, -30, -30, -30, -30, -30, 0, -30, -30, -30, -30, 0, -30, 0, -30, -30, 0, 0, 0, 0,
        // State 45
        0, -33, 0, 17, -33, -33, 0, -33, 18, 0, -33, -33, 0, 19, 20, -33, -33, -33, -33, 0, -33, -33, -33, -33, 0, -33, 0, -33, -33, 0, 0, 0, 0,
        // State 46
        0, -36, 0, 0, -36, -36, 0, -36, 0, 0, 21, 22, 0, 0, 0, -36, -36, -36, -36, 0, -36, -36, -36, -36, 0, -36, 0, -36, -36, 0, 0, 0, 0,
        // State 47
        0, -38, 0, 0, -38, -38, 0, -38, 0, 0, 0, 0, 0, 0, 0, -38, -38, 23, -38, 0, -38, -38, -38, 24, 0, -38, 0, -38, -38, 0, 0, 0, 0,
        // State 48
        0, -40, 0, 0, 25, -40, 0, -40, 0, 0, 0, 0, 0, 0, 0, -40, -40, 0, -40, 0, -40, -40, -40, 0, 0, -40, 0, -40, -40, 0, 0, 0, 0,
        // State 49
        0, -42, 0, 0, 0, -42, 0, -42, 0, 0, 0, 0, 0, 0, 0, -42, -42, 0, -42, 0, -42, -42, -42, 0, 0, 26, 0, -42, -42, 0, 0, 0, 0,
        // State 50
        0, -15, 0, 0, 0, -15, 0, -15, 0, 0, 0, 0, 0, 0, 0, -15, -15, 0, -15, 0, -15, -15, -15, 0, 0, 0, 0, 27, -15, 0, 0, 0, 0,
        // State 51
        0, -51, 0, -51, -51, -51, -51, -51, -51, -51, -51, -51, 0, -51, -51, -51, -51, -51, -51, 0, -51, -51, -51, -51, 0, -51, 0, -51, -51, -51, -51, -51, -51,
        // State 52
        0, -53, 0, -53, -53, -53, -53, -53, -53, -53, -53, -53, 0, -53, -53, -53, -53, -53, -53, 0, -53, -53, -53, -53, 0, -53, 0, -53, -53, -53, -53, -53, -53,
        // State 53
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 28, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 54
        0, -7, 0, -7, -7, -7, -7, -7, -7, -7, -7, -7, 0, -7, -7, -7, -7, -7, -7, 0, -7, -7, -7, -7, 0, -7, 0, -7, -7, -7, -7, -7, -7,
        // State 55
        0, -62, 0, -62, -62, -62, -62, -62, -62, -62, -62, -62, 0, -62, -62, -62, -62, -62, -62, 0, -62, -62, -62, -62, 0, -62, 0, -62, -62, -62, -62, -62, -62,
        // State 56
        0, -61, 0, -61, -61, -61, -61, -61, -61, -61, -61, -61, -47, -61, -61, -61, -61, -61, -61, 0, -61, -61, -61, -61, 0, -61, 0, -61, -61, -61, -61, -61, -61,
        // State 57
        0, -63, 0, -63, -63, -63, -63, -63, -63, -63, -63, -63, 0, -63, -63, -63, -63, -63, -63, 0, -63, -63, -63, -63, 0, -63, 0, -63, -63, -63, -63, -63, -63,
        // State 58
        0, -6, 0, -6, -6, -6, -6, -6, -6, -6, -6, -6, 0, -6, -6, -6, -6, -6, -6, 0, -6, -6, -6, -6, 0, -6, 0, -6, -6, -6, -6, -6, -6,
        // State 59
        0, -54, 0, -54, -54, -54, -54, -54, -54, -54, -54, -54, 0, -54, -54, -54, -54, -54, -54, 0, -54, -54, -54, -54, 0, -54, 0, -54, -54, -54, -54, -54, -54,
        // State 60
        0, -45, 0, -45, -45, -45, -45, -45, -45, -45, -45, -45, 0, -45, -45, -45, -45, -45, -45, 0, -45, -45, -45, -45, 0, -45, 0, -45, -45, -45, -45, -45, -45,
        // State 61
        0, -43, 0, -43, -43, -43, -43, -43, -43, -43, -43, -43, 0, -43, -43, -43, -43, -43, -43, 0, -43, -43, -43, -43, 0, -43, 0, -43, -43, -43, -43, -43, -43,
        // State 62
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 70, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 63
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -56, 0, 0, 0,
        // State 64
        0, -50, 0, -50, -50, -50, -50, -50, -50, -50, -50, -50, 0, -50, -50, -50, -50, -50, -50, 0, -50, -50, -50, -50, 0, -50, 0, -50, -50, -50, -50, -50, -50,
        // State 65
        0, -61, 0, -61, -61, -61, -61, -61, -61, -61, -61, -61, 0, -61, -61, -61, -61, -61, -61, 0, -61, -61, -61, -61, 0, -61, 0, -61, -61, -61, -61, -61, -61,
        // State 66
        0, -22, 0, -22, -22, -22, 0, -22, -22, -22, -22, -22, 0, -22, -22, -22, -22, -22, -22, 0, -22, -22, -22, -22, 0, -22, 0, -22, -22, 0, 0, 0, 0,
        // State 67
        0, 0, 0, 0, 0, 0, 0, 92, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 68
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 69
        0, -46, 0, -46, -46, -46, -46, -46, -46, -46, -46, -46, 0, -46, -46, -46, -46, -46, -46, 0, -46, -46, -46, -46, 0, -46, 0, -46, -46, -46, -46, -46, -46,
        // State 70
        0, -10, 0, 0, 0, -10, 0, -10, 0, 0, 0, 0, 0, 0, 0, -10, -10, 0, -10, 0, -10, -10, -10, 0, 0, 0, 0, 27, -10, 0, 0, 0, 0,
        // State 71
        0, -11, 0, 0, 0, -11, 0, -11, 0, 0, 0, 0, 0, 0, 0, -11, -11, 0, -11, 0, -11, -11, -11, 0, 0, 0, 0, 27, -11, 0, 0, 0, 0,
        // State 72
        0, -13, 0, 0, 0, -13, 0, -13, 0, 0, 0, 0, 0, 0, 0, -13, -13, 0, -13, 0, -13, -13, -13, 0, 0, 0, 0, 27, -13, 0, 0, 0, 0,
        // State 73
        0, -9, 0, 0, 0, -9, 0, -9, 0, 0, 0, 0, 0, 0, 0, -9, -9, 0, -9, 0, -9, -9, -9, 0, 0, 0, 0, 27, -9, 0, 0, 0, 0,
        // State 74
        0, -12, 0, 0, 0, -12, 0, -12, 0, 0, 0, 0, 0, 0, 0, -12, -12, 0, -12, 0, -12, -12, -12, 0, 0, 0, 0, 27, -12, 0, 0, 0, 0,
        // State 75
        0, -14, 0, 0, 0, -14, 0, -14, 0, 0, 0, 0, 0, 0, 0, -14, -14, 0, -14, 0, -14, -14, -14, 0, 0, 0, 0, 27, -14, 0, 0, 0, 0,
        // State 76
        0, 8, 0, 0, 0, -16, 0, -16, 0, 0, 0, 0, 0, 0, 0, -16, 9, 0, 10, 0, 11, 12, 13, 0, 0, 0, 0, 0, -16, 0, 0, 0, 0,
        // State 77
        0, 0, 0, 0, 0, 14, 0, -18, 0, 0, 0, 0, 0, 0, 0, -18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -18, 0, 0, 0, 0,
        // State 78
        0, -24, 0, -24, -24, -24, 0, -24, -24, 0, -24, -24, 0, -24, -24, -24, -24, -24, -24, 0, -24, -24, -24, -24, 0, -24, 0, -24, -24, 0, 0, 0, 0,
        // State 79
        0, -29, 0, -29, -29, -29, 0, -29, -29, 0, -29, -29, 0, -29, -29, -29, -29, -29, -29, 0, -29, -29, -29, -29, 0, -29, 0, -29, -29, 0, 0, 0, 0,
        // State 80
        0, -26, 0, -26, -26, -26, 0, -26, -26, 0, -26, -26, 0, -26, -26, -26, -26, -26, -26, 0, -26, -26, -26, -26, 0, -26, 0, -26, -26, 0, 0, 0, 0,
        // State 81
        0, -27, 0, -27, -27, -27, 0, -27, -27, 0, -27, -27, 0, -27, -27, -27, -27, -27, -27, 0, -27, -27, -27, -27, 0, -27, 0, -27, -27, 0, 0, 0, 0,
        // State 82
        0, -28, 0, -28, -28, -28, 0, -28, -28, 0, -28, -28, 0, -28, -28, -28, -28, -28, -28, 0, -28, -28, -28, -28, 0, -28, 0, -28, -28, 0, 0, 0, 0,
        // State 83
        0, -31, 0, 17, -31, -31, 0, -31, 18, 0, -31, -31, 0, 19, 20, -31, -31, -31, -31, 0, -31, -31, -31, -31, 0, -31, 0, -31, -31, 0, 0, 0, 0,
        // State 84
        0, -32, 0, 17, -32, -32, 0, -32, 18, 0, -32, -32, 0, 19, 20, -32, -32, -32, -32, 0, -32, -32, -32, -32, 0, -32, 0, -32, -32, 0, 0, 0, 0,
        // State 85
        0, -34, 0, 0, -34, -34, 0, -34, 0, 0, 21, 22, 0, 0, 0, -34, -34, -34, -34, 0, -34, -34, -34, -34, 0, -34, 0, -34, -34, 0, 0, 0, 0,
        // State 86
        0, -35, 0, 0, -35, -35, 0, -35, 0, 0, 21, 22, 0, 0, 0, -35, -35, -35, -35, 0, -35, -35, -35, -35, 0, -35, 0, -35, -35, 0, 0, 0, 0,
        // State 87
        0, -37, 0, 0, -37, -37, 0, -37, 0, 0, 0, 0, 0, 0, 0, -37, -37, 23, -37, 0, -37, -37, -37, 24, 0, -37, 0, -37, -37, 0, 0, 0, 0,
        // State 88
        0, -39, 0, 0, 25, -39, 0, -39, 0, 0, 0, 0, 0, 0, 0, -39, -39, 0, -39, 0, -39, -39, -39, 0, 0, -39, 0, -39, -39, 0, 0, 0, 0,
        // State 89
        0, -41, 0, 0, 0, -41, 0, -41, 0, 0, 0, 0, 0, 0, 0, -41, -41, 0, -41, 0, -41, -41, -41, 0, 0, 26, 0, -41, -41, 0, 0, 0, 0,
        // State 90
        0, 0, 0, 0, 0, 0, 0, -20, 0, 0, 0, 0, 0, 0, 0, -20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 91
        0, -55, 0, -55, -55, -55, -55, -55, -55, -55, -55, -55, 0, -55, -55, -55, -55, -55, -55, 0, -55, -55, -55, -55, 0, -55, 0, -55, -55, -55, -55, -55, -55,
        // State 92
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 93
        0, 0, 0, 0, 0, 0, 0, -4, 0, 0, 0, 0, 0, 0, 0, -4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];
    fn __action(state: i8, integer: usize) -> i8 {
        __ACTION[(state as usize) * 33 + integer]
    }
    const __EOF_ACTION: &[i8] = &[
        // State 0
        -48,
        // State 1
        -49,
        // State 2
        0,
        // State 3
        0,
        // State 4
        0,
        // State 5
        0,
        // State 6
        0,
        // State 7
        0,
        // State 8
        0,
        // State 9
        0,
        // State 10
        0,
        // State 11
        0,
        // State 12
        0,
        // State 13
        0,
        // State 14
        0,
        // State 15
        0,
        // State 16
        0,
        // State 17
        0,
        // State 18
        0,
        // State 19
        0,
        // State 20
        0,
        // State 21
        0,
        // State 22
        0,
        // State 23
        0,
        // State 24
        0,
        // State 25
        0,
        // State 26
        0,
        // State 27
        0,
        // State 28
        0,
        // State 29
        0,
        // State 30
        0,
        // State 31
        0,
        // State 32
        -64,
        // State 33
        0,
        // State 34
        -59,
        // State 35
        0,
        // State 36
        -60,
        // State 37
        0,
        // State 38
        0,
        // State 39
        0,
        // State 40
        0,
        // State 41
        0,
        // State 42
        0,
        // State 43
        0,
        // State 44
        0,
        // State 45
        0,
        // State 46
        0,
        // State 47
        0,
        // State 48
        0,
        // State 49
        0,
        // State 50
        0,
        // State 51
        0,
        // State 52
        0,
        // State 53
        0,
        // State 54
        0,
        // State 55
        0,
        // State 56
        0,
        // State 57
        0,
        // State 58
        0,
        // State 59
        0,
        // State 60
        0,
        // State 61
        0,
        // State 62
        0,
        // State 63
        -56,
        // State 64
        0,
        // State 65
        0,
        // State 66
        0,
        // State 67
        0,
        // State 68
        0,
        // State 69
        0,
        // State 70
        0,
        // State 71
        0,
        // State 72
        0,
        // State 73
        0,
        // State 74
        0,
        // State 75
        0,
        // State 76
        0,
        // State 77
        0,
        // State 78
        0,
        // State 79
        0,
        // State 80
        0,
        // State 81
        0,
        // State 82
        0,
        // State 83
        0,
        // State 84
        0,
        // State 85
        0,
        // State 86
        0,
        // State 87
        0,
        // State 88
        0,
        // State 89
        0,
        // State 90
        0,
        // State 91
        0,
        // State 92
        0,
        // State 93
        0,
    ];
    fn __goto(state: i8, nt: usize) -> i8 {
        match nt {
            3 => match state {
                5 => 67,
                29 => 93,
                _ => 37,
            },
            4 => match state {
                3 => 64,
                _ => 38,
            },
            5 => 3,
            6 => match state {
                13 => 76,
                _ => 39,
            },
            7 => match state {
                14 => 77,
                _ => 40,
            },
            8 => 41,
            9 => match state {
                27 => 90,
                28 => 92,
                _ => 42,
            },
            10 => match state {
                4 => 66,
                _ => 43,
            },
            11 => match state {
                15 => 78,
                16 => 79,
                17 => 80,
                18 => 81,
                19 => 82,
                _ => 44,
            },
            12 => match state {
                20 => 83,
                21 => 84,
                _ => 45,
            },
            13 => match state {
                22 => 85,
                23 => 86,
                _ => 46,
            },
            14 => match state {
                24 => 87,
                _ => 47,
            },
            15 => match state {
                25 => 88,
                _ => 48,
            },
            16 => match state {
                26 => 89,
                _ => 49,
            },
            17 => match state {
                7 => 70,
                8 => 71,
                9 => 72,
                10 => 73,
                11 => 74,
                12 => 75,
                _ => 50,
            },
            18 => 51,
            19 => 30,
            20 => 52,
            21 => match state {
                0..=1 => 31,
                6 => 68,
                _ => 53,
            },
            22 => 32,
            23 => 54,
            24 => 55,
            25 => match state {
                0..=1 | 6 => 33,
                3..=4 | 7..=26 => 65,
                _ => 56,
            },
            26 => 57,
            27 => 58,
            28 => match state {
                1 => 36,
                _ => 34,
            },
            30 => 1,
            31 => 59,
            _ => 0,
        }
    }
    const __TERMINAL: &[&str] = &[
        r###""!""###,
        r###""!=""###,
        r###""#.*""###,
        r###""%""###,
        r###""&""###,
        r###""&&""###,
        r###""(""###,
        r###"")""###,
        r###""*""###,
        r###""**""###,
        r###""+""###,
        r###""-""###,
        r###""->""###,
        r###""/""###,
        r###""//""###,
        r###"";""###,
        r###""<""###,
        r###""<<""###,
        r###""<=""###,
        r###""=""###,
        r###""==""###,
        r###"">""###,
        r###"">=""###,
        r###"">>""###,
        r###""Numbers can`t have leading zeros""###,
        r###""^""###,
        r###""let""###,
        r###""|""###,
        r###""||""###,
        r###"r#"(?:\\p{XID_Start}|_)\\p{XID_Continue}*"#"###,
        r###"r#"-?(?:0|[1-9]\\d*)"#"###,
        r###"r#"-?(?:0|[1-9]\\d*)\\.\\d*(?:[eE][+-]?\\d+)?"#"###,
        r###"r#"0\\d"#"###,
    ];
    fn __expected_tokens(__state: i8) -> alloc::vec::Vec<alloc::string::String> {
        __TERMINAL.iter().enumerate().filter_map(|(index, terminal)| {
            let next_state = __action(__state, index);
            if next_state == 0 {
                None
            } else {
                Some(alloc::string::ToString::to_string(terminal))
            }
        }).collect()
    }
    fn __expected_tokens_from_states<
        'input,
        '__1,
    >(
        __states: &[i8],
        _: core::marker::PhantomData<(&'input ())>,
    ) -> alloc::vec::Vec<alloc::string::String>
    {
        __TERMINAL.iter().enumerate().filter_map(|(index, terminal)| {
            if __accepts(None, __states, Some(index), core::marker::PhantomData::<(&())>) {
                Some(alloc::string::ToString::to_string(terminal))
            } else {
                None
            }
        }).collect()
    }
    struct __StateMachine<'input, '__1>
    where 
    {
        ctx: &'__1 mut crate::ParseCtx,
        input: &'input str,
        __phantom: core::marker::PhantomData<(&'input ())>,
    }
    impl<'input, '__1> __state_machine::ParserDefinition for __StateMachine<'input, '__1>
    where 
    {
        type Location = usize;
        type Error = &'static str;
        type Token = Token<'input>;
        type TokenIndex = usize;
        type Symbol = __Symbol<'input>;
        type Success = alloc::vec::Vec<ast::Tld>;
        type StateIndex = i8;
        type Action = i8;
        type ReduceIndex = i8;
        type NonterminalIndex = usize;

        #[inline]
        fn start_location(&self) -> Self::Location {
              Default::default()
        }

        #[inline]
        fn start_state(&self) -> Self::StateIndex {
              0
        }

        #[inline]
        fn token_to_index(&self, token: &Self::Token) -> Option<usize> {
            __token_to_integer(token, core::marker::PhantomData::<(&())>)
        }

        #[inline]
        fn action(&self, state: i8, integer: usize) -> i8 {
            __action(state, integer)
        }

        #[inline]
        fn error_action(&self, state: i8) -> i8 {
            __action(state, 33 - 1)
        }

        #[inline]
        fn eof_action(&self, state: i8) -> i8 {
            __EOF_ACTION[state as usize]
        }

        #[inline]
        fn goto(&self, state: i8, nt: usize) -> i8 {
            __goto(state, nt)
        }

        fn token_to_symbol(&self, token_index: usize, token: Self::Token) -> Self::Symbol {
            __token_to_symbol(token_index, token, core::marker::PhantomData::<(&())>)
        }

        fn expected_tokens(&self, state: i8) -> alloc::vec::Vec<alloc::string::String> {
            __expected_tokens(state)
        }

        fn expected_tokens_from_states(&self, states: &[i8]) -> alloc::vec::Vec<alloc::string::String> {
            __expected_tokens_from_states(states, core::marker::PhantomData::<(&())>)
        }

        #[inline]
        fn uses_error_recovery(&self) -> bool {
            false
        }

        #[inline]
        fn error_recovery_symbol(
            &self,
            recovery: __state_machine::ErrorRecovery<Self>,
        ) -> Self::Symbol {
            panic!("error recovery not enabled for this grammar")
        }

        fn reduce(
            &mut self,
            action: i8,
            start_location: Option<&Self::Location>,
            states: &mut alloc::vec::Vec<i8>,
            symbols: &mut alloc::vec::Vec<__state_machine::SymbolTriple<Self>>,
        ) -> Option<__state_machine::ParseResult<Self>> {
            __reduce(
                self.ctx,
                self.input,
                action,
                start_location,
                states,
                symbols,
                core::marker::PhantomData::<(&())>,
            )
        }

        fn simulate_reduce(&self, action: i8) -> __state_machine::SimulatedReduce<Self> {
            __simulate_reduce(action, core::marker::PhantomData::<(&())>)
        }
    }
    fn __token_to_integer<
        'input,
    >(
        __token: &Token<'input>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> Option<usize>
    {
        match *__token {
            Token(4, _) if true => Some(0),
            Token(5, _) if true => Some(1),
            Token(6, _) if true => Some(2),
            Token(7, _) if true => Some(3),
            Token(8, _) if true => Some(4),
            Token(9, _) if true => Some(5),
            Token(10, _) if true => Some(6),
            Token(11, _) if true => Some(7),
            Token(12, _) if true => Some(8),
            Token(13, _) if true => Some(9),
            Token(14, _) if true => Some(10),
            Token(15, _) if true => Some(11),
            Token(16, _) if true => Some(12),
            Token(17, _) if true => Some(13),
            Token(18, _) if true => Some(14),
            Token(19, _) if true => Some(15),
            Token(20, _) if true => Some(16),
            Token(21, _) if true => Some(17),
            Token(22, _) if true => Some(18),
            Token(23, _) if true => Some(19),
            Token(24, _) if true => Some(20),
            Token(25, _) if true => Some(21),
            Token(26, _) if true => Some(22),
            Token(27, _) if true => Some(23),
            Token(28, _) if true => Some(24),
            Token(29, _) if true => Some(25),
            Token(30, _) if true => Some(26),
            Token(31, _) if true => Some(27),
            Token(32, _) if true => Some(28),
            Token(0, _) if true => Some(29),
            Token(1, _) if true => Some(30),
            Token(2, _) if true => Some(31),
            Token(3, _) if true => Some(32),
            _ => None,
        }
    }
    fn __token_to_symbol<
        'input,
    >(
        __token_index: usize,
        __token: Token<'input>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> __Symbol<'input>
    {
        #[allow(clippy::manual_range_patterns)]match __token_index {
            0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 => match __token {
                Token(4, __tok0) | Token(5, __tok0) | Token(6, __tok0) | Token(7, __tok0) | Token(8, __tok0) | Token(9, __tok0) | Token(10, __tok0) | Token(11, __tok0) | Token(12, __tok0) | Token(13, __tok0) | Token(14, __tok0) | Token(15, __tok0) | Token(16, __tok0) | Token(17, __tok0) | Token(18, __tok0) | Token(19, __tok0) | Token(20, __tok0) | Token(21, __tok0) | Token(22, __tok0) | Token(23, __tok0) | Token(24, __tok0) | Token(25, __tok0) | Token(26, __tok0) | Token(27, __tok0) | Token(28, __tok0) | Token(29, __tok0) | Token(30, __tok0) | Token(31, __tok0) | Token(32, __tok0) | Token(0, __tok0) | Token(1, __tok0) | Token(2, __tok0) | Token(3, __tok0) if true => __Symbol::Variant0(__tok0),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
    fn __simulate_reduce<
        'input,
        '__1,
    >(
        __reduce_index: i8,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> __state_machine::SimulatedReduce<__StateMachine<'input, '__1>>
    {
        match __reduce_index {
            0 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 0,
                    nonterminal_produced: 0,
                }
            }
            1 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 0,
                    nonterminal_produced: 1,
                }
            }
            2 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 2,
                }
            }
            3 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 6,
                    nonterminal_produced: 3,
                }
            }
            4 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 3,
                }
            }
            5 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 4,
                }
            }
            6 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 5,
                }
            }
            7 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 5,
                }
            }
            8 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 6,
                }
            }
            9 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 6,
                }
            }
            10 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 6,
                }
            }
            11 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 6,
                }
            }
            12 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 6,
                }
            }
            13 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 6,
                }
            }
            14 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 6,
                }
            }
            15 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 7,
                }
            }
            16 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 7,
                }
            }
            17 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 8,
                }
            }
            18 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 8,
                }
            }
            19 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 9,
                }
            }
            20 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 9,
                }
            }
            21 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 2,
                    nonterminal_produced: 10,
                }
            }
            22 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 10,
                }
            }
            23 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 11,
                }
            }
            24 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 11,
                }
            }
            25 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 12,
                }
            }
            26 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 12,
                }
            }
            27 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 12,
                }
            }
            28 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 12,
                }
            }
            29 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 12,
                }
            }
            30 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 13,
                }
            }
            31 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 13,
                }
            }
            32 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 13,
                }
            }
            33 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 14,
                }
            }
            34 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 14,
                }
            }
            35 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 14,
                }
            }
            36 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 15,
                }
            }
            37 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 15,
                }
            }
            38 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 16,
                }
            }
            39 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 16,
                }
            }
            40 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 17,
                }
            }
            41 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 17,
                }
            }
            42 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 18,
                }
            }
            43 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 19,
                }
            }
            44 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 20,
                }
            }
            45 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 2,
                    nonterminal_produced: 20,
                }
            }
            46 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 21,
                }
            }
            47 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 0,
                    nonterminal_produced: 22,
                }
            }
            48 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 22,
                }
            }
            49 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 2,
                    nonterminal_produced: 23,
                }
            }
            50 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 24,
                }
            }
            51 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 25,
                }
            }
            52 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 26,
                }
            }
            53 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 27,
                }
            }
            54 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 27,
                }
            }
            55 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 4,
                    nonterminal_produced: 28,
                }
            }
            56 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 0,
                    nonterminal_produced: 29,
                }
            }
            57 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 29,
                }
            }
            58 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 30,
                }
            }
            59 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 2,
                    nonterminal_produced: 30,
                }
            }
            60 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 31,
                }
            }
            61 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 31,
                }
            }
            62 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 31,
                }
            }
            63 => __state_machine::SimulatedReduce::Accept,
            _ => panic!("invalid reduction index {}", __reduce_index)
        }
    }
    pub struct ScriptParser {
        builder: __lalrpop_util::lexer::MatcherBuilder,
        _priv: (),
    }

    impl Default for ScriptParser { fn default() -> Self { Self::new() } }
    impl ScriptParser {
        pub fn new() -> ScriptParser {
            let __builder = super::__intern_token::new_builder();
            ScriptParser {
                builder: __builder,
                _priv: (),
            }
        }

        #[allow(dead_code)]
        pub fn parse<
            'input,
        >(
            &self,
            ctx: &mut crate::ParseCtx,
            input: &'input str,
        ) -> Result<alloc::vec::Vec<ast::Tld>, __lalrpop_util::ParseError<usize, Token<'input>, &'static str>>
        {
            let mut __tokens = self.builder.matcher(input);
            __state_machine::Parser::drive(
                __StateMachine {
                    ctx,
                    input,
                    __phantom: core::marker::PhantomData::<(&())>,
                },
                __tokens,
            )
        }
    }
    fn __accepts<
        'input,
        '__1,
    >(
        __error_state: Option<i8>,
        __states: &[i8],
        __opt_integer: Option<usize>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> bool
    {
        let mut __states = __states.to_vec();
        __states.extend(__error_state);
        loop {
            let mut __states_len = __states.len();
            let __top = __states[__states_len - 1];
            let __action = match __opt_integer {
                None => __EOF_ACTION[__top as usize],
                Some(__integer) => __action(__top, __integer),
            };
            if __action == 0 { return false; }
            if __action > 0 { return true; }
            let (__to_pop, __nt) = match __simulate_reduce(-(__action + 1), core::marker::PhantomData::<(&())>) {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop, nonterminal_produced
                } => (states_to_pop, nonterminal_produced),
                __state_machine::SimulatedReduce::Accept => return true,
            };
            __states_len -= __to_pop;
            __states.truncate(__states_len);
            let __top = __states[__states_len - 1];
            let __next_state = __goto(__top, __nt);
            __states.push(__next_state);
        }
    }
    fn __reduce<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __action: i8,
        __lookahead_start: Option<&usize>,
        __states: &mut alloc::vec::Vec<i8>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> Option<Result<alloc::vec::Vec<ast::Tld>,__lalrpop_util::ParseError<usize, Token<'input>, &'static str>>>
    {
        let (__pop_states, __nonterminal) = match __action {
            0 => {
                __reduce0(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            1 => {
                __reduce1(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            2 => {
                __reduce2(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            3 => {
                __reduce3(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            4 => {
                __reduce4(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            5 => {
                __reduce5(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            6 => {
                __reduce6(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            7 => {
                __reduce7(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            8 => {
                __reduce8(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            9 => {
                __reduce9(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            10 => {
                __reduce10(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            11 => {
                __reduce11(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            12 => {
                __reduce12(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            13 => {
                __reduce13(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            14 => {
                __reduce14(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            15 => {
                __reduce15(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            16 => {
                __reduce16(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            17 => {
                __reduce17(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            18 => {
                __reduce18(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            19 => {
                __reduce19(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            20 => {
                __reduce20(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            21 => {
                __reduce21(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            22 => {
                __reduce22(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            23 => {
                __reduce23(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            24 => {
                __reduce24(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            25 => {
                __reduce25(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            26 => {
                __reduce26(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            27 => {
                __reduce27(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            28 => {
                __reduce28(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            29 => {
                __reduce29(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            30 => {
                __reduce30(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            31 => {
                __reduce31(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            32 => {
                __reduce32(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            33 => {
                __reduce33(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            34 => {
                __reduce34(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            35 => {
                __reduce35(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            36 => {
                __reduce36(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            37 => {
                __reduce37(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            38 => {
                __reduce38(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            39 => {
                __reduce39(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            40 => {
                __reduce40(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            41 => {
                __reduce41(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            42 => {
                __reduce42(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            43 => {
                __reduce43(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            44 => {
                __reduce44(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            45 => {
                __reduce45(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            46 => {
                __reduce46(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            47 => {
                __reduce47(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            48 => {
                __reduce48(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            49 => {
                __reduce49(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            50 => {
                __reduce50(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            51 => {
                __reduce51(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            52 => {
                __reduce52(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            53 => {
                __reduce53(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            54 => {
                __reduce54(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            55 => {
                __reduce55(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            56 => {
                __reduce56(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            57 => {
                __reduce57(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            58 => {
                __reduce58(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            59 => {
                __reduce59(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            60 => {
                __reduce60(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            61 => {
                __reduce61(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            62 => {
                __reduce62(ctx, input, __lookahead_start, __symbols, core::marker::PhantomData::<(&())>)
            }
            63 => {
                // __Script = Script => ActionFn(0);
                let __sym0 = __pop_Variant5(__symbols);
                let __start = __sym0.0;
                let __end = __sym0.2;
                let __nt = super::__action0::<>(ctx, input, __sym0);
                return Some(Ok(__nt));
            }
            _ => panic!("invalid action code {}", __action)
        };
        let __states_len = __states.len();
        __states.truncate(__states_len - __pop_states);
        let __state = *__states.last().unwrap();
        let __next_state = __goto(__state, __nonterminal);
        __states.push(__next_state);
        None
    }
    #[inline(never)]
    fn __symbol_type_mismatch() -> ! {
        panic!("symbol type mismatch")
    }
    fn __pop_Variant6<
      'input,
    >(
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, (Expr, Expr, Span), usize)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant6(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant7<
      'input,
    >(
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, (String, Span), usize)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant7(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant2<
      'input,
    >(
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, Expr, usize)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant2(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant4<
      'input,
    >(
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, Pattern, usize)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant4(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant3<
      'input,
    >(
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, String, usize)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant3(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant5<
      'input,
    >(
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, alloc::vec::Vec<ast::Tld>, usize)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant5(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant8<
      'input,
    >(
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, ast::Tld, usize)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant8(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant1<
      'input,
    >(
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, usize, usize)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant1(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant0<
      'input,
    >(
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>
    ) -> (usize, &'input str, usize)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant0(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __reduce0<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // @L =  => ActionFn(56);
        let __start = __lookahead_start.cloned().or_else(|| __symbols.last().map(|s| s.2)).unwrap_or_default();
        let __end = __start;
        let __nt = super::__action56::<>(ctx, input, &__start, &__end);
        __symbols.push((__start, __Symbol::Variant1(__nt), __end));
        (0, 0)
    }
    fn __reduce1<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // @R =  => ActionFn(55);
        let __start = __lookahead_start.cloned().or_else(|| __symbols.last().map(|s| s.2)).unwrap_or_default();
        let __end = __start;
        let __nt = super::__action55::<>(ctx, input, &__start, &__end);
        __symbols.push((__start, __Symbol::Variant1(__nt), __end));
        (0, 1)
    }
    fn __reduce2<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Comment = "#.*" => ActionFn(1);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action1::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant0(__nt), __end));
        (1, 2)
    }
    fn __reduce3<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr = "let", Pattern, "=", Expr13, ";", Expr => ActionFn(91);
        assert!(__symbols.len() >= 6);
        let __sym5 = __pop_Variant2(__symbols);
        let __sym4 = __pop_Variant0(__symbols);
        let __sym3 = __pop_Variant2(__symbols);
        let __sym2 = __pop_Variant0(__symbols);
        let __sym1 = __pop_Variant4(__symbols);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym5.2;
        let __nt = super::__action91::<>(ctx, input, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (6, 3)
    }
    fn __reduce4<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr = Expr13 => ActionFn(49);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action49::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 3)
    }
    fn __reduce5<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr0 = Term => ActionFn(11);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action11::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 4)
    }
    fn __reduce6<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr1 = Spanned2<Expr1, Expr0> => ActionFn(12);
        let __sym0 = __pop_Variant6(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action12::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 5)
    }
    fn __reduce7<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr1 = Expr0 => ActionFn(13);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action13::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 5)
    }
    fn __reduce8<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr10 = Expr10, "==", Expr9 => ActionFn(92);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action92::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 6)
    }
    fn __reduce9<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr10 = Expr10, "!=", Expr9 => ActionFn(93);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action93::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 6)
    }
    fn __reduce10<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr10 = Expr10, "<", Expr9 => ActionFn(94);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action94::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 6)
    }
    fn __reduce11<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr10 = Expr10, ">", Expr9 => ActionFn(95);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action95::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 6)
    }
    fn __reduce12<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr10 = Expr10, "<=", Expr9 => ActionFn(96);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action96::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 6)
    }
    fn __reduce13<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr10 = Expr10, ">=", Expr9 => ActionFn(97);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action97::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 6)
    }
    fn __reduce14<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr10 = Expr9 => ActionFn(41);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action41::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 6)
    }
    fn __reduce15<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr11 = Expr11, "&&", Expr10 => ActionFn(98);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action98::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 7)
    }
    fn __reduce16<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr11 = Expr10 => ActionFn(43);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action43::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 7)
    }
    fn __reduce17<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr12 = Expr12, "||", Expr11 => ActionFn(99);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action99::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 8)
    }
    fn __reduce18<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr12 = Expr11 => ActionFn(45);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action45::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 8)
    }
    fn __reduce19<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr13 = Pattern, "->", Expr13 => ActionFn(100);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant4(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action100::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 9)
    }
    fn __reduce20<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr13 = Expr12 => ActionFn(47);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action47::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 9)
    }
    fn __reduce21<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr2 = "!", Expr2 => ActionFn(101);
        assert!(__symbols.len() >= 2);
        let __sym1 = __pop_Variant2(__symbols);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym1.2;
        let __nt = super::__action101::<>(ctx, input, __sym0, __sym1);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (2, 10)
    }
    fn __reduce22<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr2 = Expr1 => ActionFn(15);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action15::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 10)
    }
    fn __reduce23<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr3 = Expr2, "**", Expr3 => ActionFn(102);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action102::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 11)
    }
    fn __reduce24<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr3 = Expr2 => ActionFn(17);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action17::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 11)
    }
    fn __reduce25<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr4 = Expr4, "*", Expr3 => ActionFn(103);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action103::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 12)
    }
    fn __reduce26<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr4 = Expr4, "/", Expr3 => ActionFn(104);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action104::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 12)
    }
    fn __reduce27<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr4 = Expr4, "//", Expr3 => ActionFn(105);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action105::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 12)
    }
    fn __reduce28<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr4 = Expr4, "%", Expr3 => ActionFn(106);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action106::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 12)
    }
    fn __reduce29<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr4 = Expr3 => ActionFn(22);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action22::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 12)
    }
    fn __reduce30<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr5 = Expr5, "+", Expr4 => ActionFn(107);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action107::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 13)
    }
    fn __reduce31<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr5 = Expr5, "-", Expr4 => ActionFn(108);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action108::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 13)
    }
    fn __reduce32<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr5 = Expr4 => ActionFn(25);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action25::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 13)
    }
    fn __reduce33<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr6 = Expr6, "<<", Expr5 => ActionFn(109);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action109::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 14)
    }
    fn __reduce34<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr6 = Expr6, ">>", Expr5 => ActionFn(110);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action110::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 14)
    }
    fn __reduce35<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr6 = Expr5 => ActionFn(28);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action28::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 14)
    }
    fn __reduce36<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr7 = Expr7, "&", Expr6 => ActionFn(111);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action111::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 15)
    }
    fn __reduce37<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr7 = Expr6 => ActionFn(30);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action30::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 15)
    }
    fn __reduce38<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr8 = Expr8, "^", Expr7 => ActionFn(112);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action112::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 16)
    }
    fn __reduce39<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr8 = Expr7 => ActionFn(32);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action32::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 16)
    }
    fn __reduce40<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr9 = Expr9, "|", Expr8 => ActionFn(113);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action113::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 17)
    }
    fn __reduce41<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Expr9 = Expr8 => ActionFn(34);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action34::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 17)
    }
    fn __reduce42<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // FloatLiteral = r#"-?(?:0|[1-9]\\d*)\\.\\d*(?:[eE][+-]?\\d+)?"# => ActionFn(5);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action5::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant3(__nt), __end));
        (1, 18)
    }
    fn __reduce43<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Ident = r#"(?:\\p{XID_Start}|_)\\p{XID_Continue}*"# => ActionFn(2);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action2::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant3(__nt), __end));
        (1, 19)
    }
    fn __reduce44<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // IntLiteral = r#"-?(?:0|[1-9]\\d*)"# => ActionFn(3);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action3::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant3(__nt), __end));
        (1, 20)
    }
    fn __reduce45<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // IntLiteral = r#"0\\d"#, "Numbers can`t have leading zeros" => ActionFn(4);
        assert!(__symbols.len() >= 2);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym1.2;
        let __nt = super::__action4::<>(ctx, input, __sym0, __sym1);
        __symbols.push((__start, __Symbol::Variant3(__nt), __end));
        (2, 20)
    }
    fn __reduce46<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Pattern = Spanned<Ident> => ActionFn(50);
        let __sym0 = __pop_Variant7(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action50::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant4(__nt), __end));
        (1, 21)
    }
    fn __reduce47<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Script =  => ActionFn(119);
        let __start = __lookahead_start.cloned().or_else(|| __symbols.last().map(|s| s.2)).unwrap_or_default();
        let __end = __start;
        let __nt = super::__action119::<>(ctx, input, &__start, &__end);
        __symbols.push((__start, __Symbol::Variant5(__nt), __end));
        (0, 22)
    }
    fn __reduce48<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Script = TopLevelDecl+ => ActionFn(120);
        let __sym0 = __pop_Variant5(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action120::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant5(__nt), __end));
        (1, 22)
    }
    fn __reduce49<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Spanned2<Expr1, Expr0> = Expr1, Expr0 => ActionFn(114);
        assert!(__symbols.len() >= 2);
        let __sym1 = __pop_Variant2(__symbols);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym1.2;
        let __nt = super::__action114::<>(ctx, input, __sym0, __sym1);
        __symbols.push((__start, __Symbol::Variant6(__nt), __end));
        (2, 23)
    }
    fn __reduce50<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Spanned<FloatLiteral> = FloatLiteral => ActionFn(115);
        let __sym0 = __pop_Variant3(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action115::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant7(__nt), __end));
        (1, 24)
    }
    fn __reduce51<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Spanned<Ident> = Ident => ActionFn(116);
        let __sym0 = __pop_Variant3(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action116::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant7(__nt), __end));
        (1, 25)
    }
    fn __reduce52<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Spanned<IntLiteral> = IntLiteral => ActionFn(117);
        let __sym0 = __pop_Variant3(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action117::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant7(__nt), __end));
        (1, 26)
    }
    fn __reduce53<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Term = VarOrLit => ActionFn(9);
        let __sym0 = __pop_Variant2(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action9::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 27)
    }
    fn __reduce54<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // Term = "(", Expr, ")" => ActionFn(10);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant0(__symbols);
        let __sym1 = __pop_Variant2(__symbols);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action10::<>(ctx, input, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (3, 27)
    }
    fn __reduce55<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // TopLevelDecl = Pattern, "=", Expr, ";" => ActionFn(118);
        assert!(__symbols.len() >= 4);
        let __sym3 = __pop_Variant0(__symbols);
        let __sym2 = __pop_Variant2(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant4(__symbols);
        let __start = __sym0.0;
        let __end = __sym3.2;
        let __nt = super::__action118::<>(ctx, input, __sym0, __sym1, __sym2, __sym3);
        __symbols.push((__start, __Symbol::Variant8(__nt), __end));
        (4, 28)
    }
    fn __reduce56<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // TopLevelDecl* =  => ActionFn(53);
        let __start = __lookahead_start.cloned().or_else(|| __symbols.last().map(|s| s.2)).unwrap_or_default();
        let __end = __start;
        let __nt = super::__action53::<>(ctx, input, &__start, &__end);
        __symbols.push((__start, __Symbol::Variant5(__nt), __end));
        (0, 29)
    }
    fn __reduce57<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // TopLevelDecl* = TopLevelDecl+ => ActionFn(54);
        let __sym0 = __pop_Variant5(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action54::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant5(__nt), __end));
        (1, 29)
    }
    fn __reduce58<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // TopLevelDecl+ = TopLevelDecl => ActionFn(61);
        let __sym0 = __pop_Variant8(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action61::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant5(__nt), __end));
        (1, 30)
    }
    fn __reduce59<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // TopLevelDecl+ = TopLevelDecl+, TopLevelDecl => ActionFn(62);
        assert!(__symbols.len() >= 2);
        let __sym1 = __pop_Variant8(__symbols);
        let __sym0 = __pop_Variant5(__symbols);
        let __start = __sym0.0;
        let __end = __sym1.2;
        let __nt = super::__action62::<>(ctx, input, __sym0, __sym1);
        __symbols.push((__start, __Symbol::Variant5(__nt), __end));
        (2, 30)
    }
    fn __reduce60<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // VarOrLit = Spanned<Ident> => ActionFn(6);
        let __sym0 = __pop_Variant7(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action6::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 31)
    }
    fn __reduce61<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // VarOrLit = Spanned<FloatLiteral> => ActionFn(7);
        let __sym0 = __pop_Variant7(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action7::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 31)
    }
    fn __reduce62<
        'input,
    >(
        ctx: &mut crate::ParseCtx,
        input: &'input str,
        __lookahead_start: Option<&usize>,
        __symbols: &mut alloc::vec::Vec<(usize,__Symbol<'input>,usize)>,
        _: core::marker::PhantomData<(&'input ())>,
    ) -> (usize, usize)
    {
        // VarOrLit = Spanned<IntLiteral> => ActionFn(8);
        let __sym0 = __pop_Variant7(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action8::<>(ctx, input, __sym0);
        __symbols.push((__start, __Symbol::Variant2(__nt), __end));
        (1, 31)
    }
}
#[allow(unused_imports)]
pub use self::__parse__Script::ScriptParser;
#[rustfmt::skip]
mod __intern_token {
    #![allow(unused_imports)]
    use super::super::ast::{self, Expr, Pattern};
    use super::super::Span;
    #[allow(unused_extern_crates)]
    extern crate lalrpop_util as __lalrpop_util;
    #[allow(unused_imports)]
    use self::__lalrpop_util::state_machine as __state_machine;
    extern crate core;
    extern crate alloc;
    pub fn new_builder() -> __lalrpop_util::lexer::MatcherBuilder {
        let __strs: &[(&str, bool)] = &[
            ("(?:(?:[A-Za-zªµºÀ-ÖØ-öø-ˁˆ-ˑˠ-ˤˬˮͰ-ʹͶͷͻ-ͽͿΆΈ-ΊΌΎ-ΡΣ-ϵϷ-ҁҊ-ԯԱ-Ֆՙՠ-ֈא-תׯ-ײؠ-يٮٯٱ-ۓەۥۦۮۯۺ-ۼۿܐܒ-ܯݍ-ޥޱߊ-ߪߴߵߺࠀ-ࠕࠚࠤࠨࡀ-ࡘࡠ-ࡪࡰ-ࢇࢉ-ࢎࢠ-ࣉऄ-हऽॐक़-ॡॱ-ঀঅ-ঌএঐও-নপ-রলশ-হঽৎড়ঢ়য়-ৡৰৱৼਅ-ਊਏਐਓ-ਨਪ-ਰਲਲ਼ਵਸ਼ਸਹਖ਼-ੜਫ਼ੲ-ੴઅ-ઍએ-ઑઓ-નપ-રલળવ-હઽૐૠૡૹଅ-ଌଏଐଓ-ନପ-ରଲଳଵ-ହଽଡ଼ଢ଼ୟ-ୡୱஃஅ-ஊஎ-ஐஒ-கஙசஜஞடணதந-பம-ஹௐఅ-ఌఎ-ఐఒ-నప-హఽౘ-ౚౝౠౡಀಅ-ಌಎ-ಐಒ-ನಪ-ಳವ-ಹಽೝೞೠೡೱೲഄ-ഌഎ-ഐഒ-ഺഽൎൔ-ൖൟ-ൡൺ-ൿඅ-ඖක-නඳ-රලව-ෆก-ะาเ-ๆກຂຄຆ-ຊຌ-ຣລວ-ະາຽເ-ໄໆໜ-ໟༀཀ-ཇཉ-ཬྈ-ྌက-ဪဿၐ-ၕၚ-ၝၡၥၦၮ-ၰၵ-ႁႎႠ-ჅჇჍა-ჺჼ-ቈቊ-ቍቐ-ቖቘቚ-ቝበ-ኈኊ-ኍነ-ኰኲ-ኵኸ-ኾዀዂ-ዅወ-ዖዘ-ጐጒ-ጕጘ-ፚᎀ-ᎏᎠ-Ᏽᏸ-ᏽᐁ-ᙬᙯ-ᙿᚁ-ᚚᚠ-ᛪᛮ-ᛸᜀ-ᜑᜟ-ᜱᝀ-ᝑᝠ-ᝬᝮ-ᝰក-ឳៗៜᠠ-ᡸᢀ-ᢨᢪᢰ-ᣵᤀ-ᤞᥐ-ᥭᥰ-ᥴᦀ-ᦫᦰ-ᧉᨀ-ᨖᨠ-ᩔᪧᬅ-ᬳᭅ-ᭌᮃ-ᮠᮮᮯᮺ-ᯥᰀ-ᰣᱍ-ᱏᱚ-ᱽᲀ-ᲈᲐ-ᲺᲽ-Ჿᳩ-ᳬᳮ-ᳳᳵᳶᳺᴀ-ᶿḀ-ἕἘ-Ἕἠ-ὅὈ-Ὅὐ-ὗὙὛὝὟ-ώᾀ-ᾴᾶ-ᾼιῂ-ῄῆ-ῌῐ-ΐῖ-Ίῠ-Ῥῲ-ῴῶ-ῼⁱⁿₐ-ₜℂℇℊ-ℓℕ℘-ℝℤΩℨK-ℹℼ-ℿⅅ-ⅉⅎⅠ-ↈⰀ-ⳤⳫ-ⳮⳲⳳⴀ-ⴥⴧⴭⴰ-ⵧⵯⶀ-ⶖⶠ-ⶦⶨ-ⶮⶰ-ⶶⶸ-ⶾⷀ-ⷆⷈ-ⷎⷐ-ⷖⷘ-ⷞ々-〇〡-〩〱-〵〸-〼ぁ-ゖゝ-ゟァ-ヺー-ヿㄅ-ㄯㄱ-ㆎㆠ-ㆿㇰ-ㇿ㐀-䶿一-ꒌꓐ-ꓽꔀ-ꘌꘐ-ꘟꘪꘫꙀ-ꙮꙿ-ꚝꚠ-ꛯꜗ-ꜟꜢ-ꞈꞋ-ꟊꟐꟑꟓꟕ-ꟙꟲ-ꠁꠃ-ꠅꠇ-ꠊꠌ-ꠢꡀ-ꡳꢂ-ꢳꣲ-ꣷꣻꣽꣾꤊ-ꤥꤰ-ꥆꥠ-ꥼꦄ-ꦲꧏꧠ-ꧤꧦ-ꧯꧺ-ꧾꨀ-ꨨꩀ-ꩂꩄ-ꩋꩠ-ꩶꩺꩾ-ꪯꪱꪵꪶꪹ-ꪽꫀꫂꫛ-ꫝꫠ-ꫪꫲ-ꫴꬁ-ꬆꬉ-ꬎꬑ-ꬖꬠ-ꬦꬨ-ꬮꬰ-ꭚꭜ-ꭩꭰ-ꯢ가-힣ힰ-ퟆퟋ-ퟻ豈-舘並-龎ﬀ-ﬆﬓ-ﬗיִײַ-ﬨשׁ-זּטּ-לּמּנּסּףּפּצּ-ﮱﯓ-ﱝﱤ-ﴽﵐ-ﶏﶒ-ﷇﷰ-ﷹﹱﹳﹷﹹﹻﹽﹿ-ﻼＡ-Ｚａ-ｚｦ-ﾝﾠ-ﾾￂ-ￇￊ-ￏￒ-ￗￚ-ￜ𐀀-𐀋𐀍-𐀦𐀨-𐀺𐀼𐀽𐀿-𐁍𐁐-𐁝𐂀-𐃺𐅀-𐅴𐊀-𐊜𐊠-𐋐𐌀-𐌟𐌭-𐍊𐍐-𐍵𐎀-𐎝𐎠-𐏃𐏈-𐏏𐏑-𐏕𐐀-𐒝𐒰-𐓓𐓘-𐓻𐔀-𐔧𐔰-𐕣𐕰-𐕺𐕼-𐖊𐖌-𐖒𐖔𐖕𐖗-𐖡𐖣-𐖱𐖳-𐖹𐖻𐖼𐘀-𐜶𐝀-𐝕𐝠-𐝧𐞀-𐞅𐞇-𐞰𐞲-𐞺𐠀-𐠅𐠈𐠊-𐠵𐠷𐠸𐠼𐠿-𐡕𐡠-𐡶𐢀-𐢞𐣠-𐣲𐣴𐣵𐤀-𐤕𐤠-𐤹𐦀-𐦷𐦾𐦿𐨀𐨐-𐨓𐨕-𐨗𐨙-𐨵𐩠-𐩼𐪀-𐪜𐫀-𐫇𐫉-𐫤𐬀-𐬵𐭀-𐭕𐭠-𐭲𐮀-𐮑𐰀-𐱈𐲀-𐲲𐳀-𐳲𐴀-𐴣𐺀-𐺩𐺰𐺱𐼀-𐼜𐼧𐼰-𐽅𐽰-𐾁𐾰-𐿄𐿠-𐿶𑀃-𑀷𑁱𑁲𑁵𑂃-𑂯𑃐-𑃨𑄃-𑄦𑅄𑅇𑅐-𑅲𑅶𑆃-𑆲𑇁-𑇄𑇚𑇜𑈀-𑈑𑈓-𑈫𑈿𑉀𑊀-𑊆𑊈𑊊-𑊍𑊏-𑊝𑊟-𑊨𑊰-𑋞𑌅-𑌌𑌏𑌐𑌓-𑌨𑌪-𑌰𑌲𑌳𑌵-𑌹𑌽𑍐𑍝-𑍡𑐀-𑐴𑑇-𑑊𑑟-𑑡𑒀-𑒯𑓄𑓅𑓇𑖀-𑖮𑗘-𑗛𑘀-𑘯𑙄𑚀-𑚪𑚸𑜀-𑜚𑝀-𑝆𑠀-𑠫𑢠-𑣟𑣿-𑤆𑤉𑤌-𑤓𑤕𑤖𑤘-𑤯𑤿𑥁𑦠-𑦧𑦪-𑧐𑧡𑧣𑨀𑨋-𑨲𑨺𑩐𑩜-𑪉𑪝𑪰-𑫸𑰀-𑰈𑰊-𑰮𑱀𑱲-𑲏𑴀-𑴆𑴈𑴉𑴋-𑴰𑵆𑵠-𑵥𑵧𑵨𑵪-𑶉𑶘𑻠-𑻲𑼂𑼄-𑼐𑼒-𑼳𑾰𒀀-𒎙𒐀-𒑮𒒀-𒕃𒾐-𒿰𓀀-𓐯𓑁-𓑆𔐀-𔙆𖠀-𖨸𖩀-𖩞𖩰-𖪾𖫐-𖫭𖬀-𖬯𖭀-𖭃𖭣-𖭷𖭽-𖮏𖹀-𖹿𖼀-𖽊𖽐𖾓-𖾟𖿠𖿡𖿣𗀀-𘟷𘠀-𘳕𘴀-𘴈𚿰-𚿳𚿵-𚿻𚿽𚿾𛀀-𛄢𛄲𛅐-𛅒𛅕𛅤-𛅧𛅰-𛋻𛰀-𛱪𛱰-𛱼𛲀-𛲈𛲐-𛲙𝐀-𝑔𝑖-𝒜𝒞𝒟𝒢𝒥𝒦𝒩-𝒬𝒮-𝒹𝒻𝒽-𝓃𝓅-𝔅𝔇-𝔊𝔍-𝔔𝔖-𝔜𝔞-𝔹𝔻-𝔾𝕀-𝕄𝕆𝕊-𝕐𝕒-𝚥𝚨-𝛀𝛂-𝛚𝛜-𝛺𝛼-𝜔𝜖-𝜴𝜶-𝝎𝝐-𝝮𝝰-𝞈𝞊-𝞨𝞪-𝟂𝟄-𝟋𝼀-𝼞𝼥-𝼪𞀰-𞁭𞄀-𞄬𞄷-𞄽𞅎𞊐-𞊭𞋀-𞋫𞓐-𞓫𞟠-𞟦𞟨-𞟫𞟭𞟮𞟰-𞟾𞠀-𞣄𞤀-𞥃𞥋𞸀-𞸃𞸅-𞸟𞸡𞸢𞸤𞸧𞸩-𞸲𞸴-𞸷𞸹𞸻𞹂𞹇𞹉𞹋𞹍-𞹏𞹑𞹒𞹔𞹗𞹙𞹛𞹝𞹟𞹡𞹢𞹤𞹧-𞹪𞹬-𞹲𞹴-𞹷𞹹-𞹼𞹾𞺀-𞺉𞺋-𞺛𞺡-𞺣𞺥-𞺩𞺫-𞺻𠀀-𪛟𪜀-𫜹𫝀-𫠝𫠠-𬺡𬺰-𮯠丽-𪘀𰀀-𱍊𱍐-𲎯]|_)[0-9A-Z_a-zªµ·ºÀ-ÖØ-öø-ˁˆ-ˑˠ-ˤˬˮ\u{300}-ʹͶͷͻ-ͽͿΆ-ΊΌΎ-ΡΣ-ϵϷ-ҁ\u{483}-\u{487}Ҋ-ԯԱ-Ֆՙՠ-ֈ\u{591}-\u{5bd}\u{5bf}\u{5c1}\u{5c2}\u{5c4}\u{5c5}\u{5c7}א-תׯ-ײ\u{610}-\u{61a}ؠ-٩ٮ-ۓە-\u{6dc}\u{6df}-\u{6e8}\u{6ea}-ۼۿܐ-\u{74a}ݍ-ޱ߀-ߵߺ\u{7fd}ࠀ-\u{82d}ࡀ-\u{85b}ࡠ-ࡪࡰ-ࢇࢉ-ࢎ\u{898}-\u{8e1}\u{8e3}-\u{963}०-९ॱ-ঃঅ-ঌএঐও-নপ-রলশ-হ\u{9bc}-\u{9c4}েৈো-ৎ\u{9d7}ড়ঢ়য়-\u{9e3}০-ৱৼ\u{9fe}\u{a01}-ਃਅ-ਊਏਐਓ-ਨਪ-ਰਲਲ਼ਵਸ਼ਸਹ\u{a3c}ਾ-\u{a42}\u{a47}\u{a48}\u{a4b}-\u{a4d}\u{a51}ਖ਼-ੜਫ਼੦-\u{a75}\u{a81}-ઃઅ-ઍએ-ઑઓ-નપ-રલળવ-હ\u{abc}-\u{ac5}\u{ac7}-ૉો-\u{acd}ૐૠ-\u{ae3}૦-૯ૹ-\u{aff}\u{b01}-ଃଅ-ଌଏଐଓ-ନପ-ରଲଳଵ-ହ\u{b3c}-\u{b44}େୈୋ-\u{b4d}\u{b55}-\u{b57}ଡ଼ଢ଼ୟ-\u{b63}୦-୯ୱ\u{b82}ஃஅ-ஊஎ-ஐஒ-கஙசஜஞடணதந-பம-ஹ\u{bbe}-ூெ-ைொ-\u{bcd}ௐ\u{bd7}௦-௯\u{c00}-ఌఎ-ఐఒ-నప-హ\u{c3c}-ౄ\u{c46}-\u{c48}\u{c4a}-\u{c4d}\u{c55}\u{c56}ౘ-ౚౝౠ-\u{c63}౦-౯ಀ-ಃಅ-ಌಎ-ಐಒ-ನಪ-ಳವ-ಹ\u{cbc}-ೄ\u{cc6}-ೈೊ-\u{ccd}\u{cd5}\u{cd6}ೝೞೠ-\u{ce3}೦-೯ೱ-ೳ\u{d00}-ഌഎ-ഐഒ-\u{d44}െ-ൈൊ-ൎൔ-\u{d57}ൟ-\u{d63}൦-൯ൺ-ൿ\u{d81}-ඃඅ-ඖක-නඳ-රලව-ෆ\u{dca}\u{dcf}-\u{dd4}\u{dd6}ෘ-\u{ddf}෦-෯ෲෳก-\u{e3a}เ-\u{e4e}๐-๙ກຂຄຆ-ຊຌ-ຣລວ-ຽເ-ໄໆ\u{ec8}-\u{ece}໐-໙ໜ-ໟༀ\u{f18}\u{f19}༠-༩\u{f35}\u{f37}\u{f39}༾-ཇཉ-ཬ\u{f71}-\u{f84}\u{f86}-\u{f97}\u{f99}-\u{fbc}\u{fc6}က-၉ၐ-\u{109d}Ⴀ-ჅჇჍა-ჺჼ-ቈቊ-ቍቐ-ቖቘቚ-ቝበ-ኈኊ-ኍነ-ኰኲ-ኵኸ-ኾዀዂ-ዅወ-ዖዘ-ጐጒ-ጕጘ-ፚ\u{135d}-\u{135f}፩-፱ᎀ-ᎏᎠ-Ᏽᏸ-ᏽᐁ-ᙬᙯ-ᙿᚁ-ᚚᚠ-ᛪᛮ-ᛸᜀ-᜕ᜟ-᜴ᝀ-\u{1753}ᝠ-ᝬᝮ-ᝰ\u{1772}\u{1773}ក-\u{17d3}ៗៜ\u{17dd}០-៩\u{180b}-\u{180d}\u{180f}-᠙ᠠ-ᡸᢀ-ᢪᢰ-ᣵᤀ-ᤞ\u{1920}-ᤫᤰ-\u{193b}᥆-ᥭᥰ-ᥴᦀ-ᦫᦰ-ᧉ᧐-᧚ᨀ-\u{1a1b}ᨠ-\u{1a5e}\u{1a60}-\u{1a7c}\u{1a7f}-᪉᪐-᪙ᪧ\u{1ab0}-\u{1abd}\u{1abf}-\u{1ace}\u{1b00}-ᭌ᭐-᭙\u{1b6b}-\u{1b73}\u{1b80}-᯳ᰀ-\u{1c37}᱀-᱉ᱍ-ᱽᲀ-ᲈᲐ-ᲺᲽ-Ჿ\u{1cd0}-\u{1cd2}\u{1cd4}-ᳺᴀ-ἕἘ-Ἕἠ-ὅὈ-Ὅὐ-ὗὙὛὝὟ-ώᾀ-ᾴᾶ-ᾼιῂ-ῄῆ-ῌῐ-ΐῖ-Ίῠ-Ῥῲ-ῴῶ-ῼ‿⁀⁔ⁱⁿₐ-ₜ\u{20d0}-\u{20dc}\u{20e1}\u{20e5}-\u{20f0}ℂℇℊ-ℓℕ℘-ℝℤΩℨK-ℹℼ-ℿⅅ-ⅉⅎⅠ-ↈⰀ-ⳤⳫ-ⳳⴀ-ⴥⴧⴭⴰ-ⵧⵯ\u{2d7f}-ⶖⶠ-ⶦⶨ-ⶮⶰ-ⶶⶸ-ⶾⷀ-ⷆⷈ-ⷎⷐ-ⷖⷘ-ⷞ\u{2de0}-\u{2dff}々-〇〡-\u{302f}〱-〵〸-〼ぁ-ゖ\u{3099}\u{309a}ゝ-ゟァ-ヺー-ヿㄅ-ㄯㄱ-ㆎㆠ-ㆿㇰ-ㇿ㐀-䶿一-ꒌꓐ-ꓽꔀ-ꘌꘐ-ꘫꙀ-\u{a66f}\u{a674}-\u{a67d}ꙿ-\u{a6f1}ꜗ-ꜟꜢ-ꞈꞋ-ꟊꟐꟑꟓꟕ-ꟙꟲ-ꠧ\u{a82c}ꡀ-ꡳꢀ-\u{a8c5}꣐-꣙\u{a8e0}-ꣷꣻꣽ-\u{a92d}ꤰ-꥓ꥠ-ꥼ\u{a980}-꧀ꧏ-꧙ꧠ-ꧾꨀ-\u{aa36}ꩀ-ꩍ꩐-꩙ꩠ-ꩶꩺ-ꫂꫛ-ꫝꫠ-ꫯꫲ-\u{aaf6}ꬁ-ꬆꬉ-ꬎꬑ-ꬖꬠ-ꬦꬨ-ꬮꬰ-ꭚꭜ-ꭩꭰ-ꯪ꯬\u{abed}꯰-꯹가-힣ힰ-ퟆퟋ-ퟻ豈-舘並-龎ﬀ-ﬆﬓ-ﬗיִ-ﬨשׁ-זּטּ-לּמּנּסּףּפּצּ-ﮱﯓ-ﱝﱤ-ﴽﵐ-ﶏﶒ-ﷇﷰ-ﷹ\u{fe00}-\u{fe0f}\u{fe20}-\u{fe2f}︳︴﹍-﹏ﹱﹳﹷﹹﹻﹽﹿ-ﻼ０-９Ａ-Ｚ＿ａ-ｚｦ-ﾾￂ-ￇￊ-ￏￒ-ￗￚ-ￜ𐀀-𐀋𐀍-𐀦𐀨-𐀺𐀼𐀽𐀿-𐁍𐁐-𐁝𐂀-𐃺𐅀-𐅴\u{101fd}𐊀-𐊜𐊠-𐋐\u{102e0}𐌀-𐌟𐌭-𐍊𐍐-\u{1037a}𐎀-𐎝𐎠-𐏃𐏈-𐏏𐏑-𐏕𐐀-𐒝𐒠-𐒩𐒰-𐓓𐓘-𐓻𐔀-𐔧𐔰-𐕣𐕰-𐕺𐕼-𐖊𐖌-𐖒𐖔𐖕𐖗-𐖡𐖣-𐖱𐖳-𐖹𐖻𐖼𐘀-𐜶𐝀-𐝕𐝠-𐝧𐞀-𐞅𐞇-𐞰𐞲-𐞺𐠀-𐠅𐠈𐠊-𐠵𐠷𐠸𐠼𐠿-𐡕𐡠-𐡶𐢀-𐢞𐣠-𐣲𐣴𐣵𐤀-𐤕𐤠-𐤹𐦀-𐦷𐦾𐦿𐨀-\u{10a03}\u{10a05}\u{10a06}\u{10a0c}-𐨓𐨕-𐨗𐨙-𐨵\u{10a38}-\u{10a3a}\u{10a3f}𐩠-𐩼𐪀-𐪜𐫀-𐫇𐫉-\u{10ae6}𐬀-𐬵𐭀-𐭕𐭠-𐭲𐮀-𐮑𐰀-𐱈𐲀-𐲲𐳀-𐳲𐴀-\u{10d27}𐴰-𐴹𐺀-𐺩\u{10eab}\u{10eac}𐺰𐺱\u{10efd}-𐼜𐼧𐼰-\u{10f50}𐽰-\u{10f85}𐾰-𐿄𐿠-𐿶𑀀-\u{11046}𑁦-𑁵\u{1107f}-\u{110ba}\u{110c2}𑃐-𑃨𑃰-𑃹\u{11100}-\u{11134}𑄶-𑄿𑅄-𑅇𑅐-\u{11173}𑅶\u{11180}-𑇄\u{111c9}-\u{111cc}𑇎-𑇚𑇜𑈀-𑈑𑈓-\u{11237}\u{1123e}-\u{11241}𑊀-𑊆𑊈𑊊-𑊍𑊏-𑊝𑊟-𑊨𑊰-\u{112ea}𑋰-𑋹\u{11300}-𑌃𑌅-𑌌𑌏𑌐𑌓-𑌨𑌪-𑌰𑌲𑌳𑌵-𑌹\u{1133b}-𑍄𑍇𑍈𑍋-𑍍𑍐\u{11357}𑍝-𑍣\u{11366}-\u{1136c}\u{11370}-\u{11374}𑐀-𑑊𑑐-𑑙\u{1145e}-𑑡𑒀-𑓅𑓇𑓐-𑓙𑖀-\u{115b5}𑖸-\u{115c0}𑗘-\u{115dd}𑘀-\u{11640}𑙄𑙐-𑙙𑚀-𑚸𑛀-𑛉𑜀-𑜚\u{1171d}-\u{1172b}𑜰-𑜹𑝀-𑝆𑠀-\u{1183a}𑢠-𑣩𑣿-𑤆𑤉𑤌-𑤓𑤕𑤖𑤘-𑤵𑤷𑤸\u{1193b}-\u{11943}𑥐-𑥙𑦠-𑦧𑦪-\u{119d7}\u{119da}-𑧡𑧣𑧤𑨀-\u{11a3e}\u{11a47}𑩐-\u{11a99}𑪝𑪰-𑫸𑰀-𑰈𑰊-\u{11c36}\u{11c38}-𑱀𑱐-𑱙𑱲-𑲏\u{11c92}-\u{11ca7}𑲩-\u{11cb6}𑴀-𑴆𑴈𑴉𑴋-\u{11d36}\u{11d3a}\u{11d3c}\u{11d3d}\u{11d3f}-\u{11d47}𑵐-𑵙𑵠-𑵥𑵧𑵨𑵪-𑶎\u{11d90}\u{11d91}𑶓-𑶘𑶠-𑶩𑻠-𑻶\u{11f00}-𑼐𑼒-\u{11f3a}𑼾-\u{11f42}𑽐-𑽙𑾰𒀀-𒎙𒐀-𒑮𒒀-𒕃𒾐-𒿰𓀀-𓐯\u{13440}-\u{13455}𔐀-𔙆𖠀-𖨸𖩀-𖩞𖩠-𖩩𖩰-𖪾𖫀-𖫉𖫐-𖫭\u{16af0}-\u{16af4}𖬀-\u{16b36}𖭀-𖭃𖭐-𖭙𖭣-𖭷𖭽-𖮏𖹀-𖹿𖼀-𖽊\u{16f4f}-𖾇\u{16f8f}-𖾟𖿠𖿡𖿣\u{16fe4}𖿰𖿱𗀀-𘟷𘠀-𘳕𘴀-𘴈𚿰-𚿳𚿵-𚿻𚿽𚿾𛀀-𛄢𛄲𛅐-𛅒𛅕𛅤-𛅧𛅰-𛋻𛰀-𛱪𛱰-𛱼𛲀-𛲈𛲐-𛲙\u{1bc9d}\u{1bc9e}\u{1cf00}-\u{1cf2d}\u{1cf30}-\u{1cf46}\u{1d165}-\u{1d169}𝅭-\u{1d172}\u{1d17b}-\u{1d182}\u{1d185}-\u{1d18b}\u{1d1aa}-\u{1d1ad}\u{1d242}-\u{1d244}𝐀-𝑔𝑖-𝒜𝒞𝒟𝒢𝒥𝒦𝒩-𝒬𝒮-𝒹𝒻𝒽-𝓃𝓅-𝔅𝔇-𝔊𝔍-𝔔𝔖-𝔜𝔞-𝔹𝔻-𝔾𝕀-𝕄𝕆𝕊-𝕐𝕒-𝚥𝚨-𝛀𝛂-𝛚𝛜-𝛺𝛼-𝜔𝜖-𝜴𝜶-𝝎𝝐-𝝮𝝰-𝞈𝞊-𝞨𝞪-𝟂𝟄-𝟋𝟎-𝟿\u{1da00}-\u{1da36}\u{1da3b}-\u{1da6c}\u{1da75}\u{1da84}\u{1da9b}-\u{1da9f}\u{1daa1}-\u{1daaf}𝼀-𝼞𝼥-𝼪\u{1e000}-\u{1e006}\u{1e008}-\u{1e018}\u{1e01b}-\u{1e021}\u{1e023}\u{1e024}\u{1e026}-\u{1e02a}𞀰-𞁭\u{1e08f}𞄀-𞄬\u{1e130}-𞄽𞅀-𞅉𞅎𞊐-\u{1e2ae}𞋀-𞋹𞓐-𞓹𞟠-𞟦𞟨-𞟫𞟭𞟮𞟰-𞟾𞠀-𞣄\u{1e8d0}-\u{1e8d6}𞤀-𞥋𞥐-𞥙𞸀-𞸃𞸅-𞸟𞸡𞸢𞸤𞸧𞸩-𞸲𞸴-𞸷𞸹𞸻𞹂𞹇𞹉𞹋𞹍-𞹏𞹑𞹒𞹔𞹗𞹙𞹛𞹝𞹟𞹡𞹢𞹤𞹧-𞹪𞹬-𞹲𞹴-𞹷𞹹-𞹼𞹾𞺀-𞺉𞺋-𞺛𞺡-𞺣𞺥-𞺩𞺫-𞺻🯰-🯹𠀀-𪛟𪜀-𫜹𫝀-𫠝𫠠-𬺡𬺰-𮯠丽-𪘀𰀀-𱍊𱍐-𲎯\u{e0100}-\u{e01ef}]*)", false),
            ("(?:\\-?(?:0|(?:[1-9][0-9٠-٩۰-۹߀-߉०-९০-৯੦-੯૦-૯୦-୯௦-௯౦-౯೦-೯൦-൯෦-෯๐-๙໐-໙༠-༩၀-၉႐-႙០-៩᠐-᠙᥆-᥏᧐-᧙᪀-᪉᪐-᪙᭐-᭙᮰-᮹᱀-᱉᱐-᱙꘠-꘩꣐-꣙꤀-꤉꧐-꧙꧰-꧹꩐-꩙꯰-꯹０-９𐒠-𐒩𐴰-𐴹𑁦-𑁯𑃰-𑃹𑄶-𑄿𑇐-𑇙𑋰-𑋹𑑐-𑑙𑓐-𑓙𑙐-𑙙𑛀-𑛉𑜰-𑜹𑣠-𑣩𑥐-𑥙𑱐-𑱙𑵐-𑵙𑶠-𑶩𑽐-𑽙𖩠-𖩩𖫀-𖫉𖭐-𖭙𝟎-𝟿𞅀-𞅉𞋰-𞋹𞓰-𞓹𞥐-𞥙🯰-🯹]*)))", false),
            ("(?:\\-?(?:0|(?:[1-9][0-9٠-٩۰-۹߀-߉०-९০-৯੦-੯૦-૯୦-୯௦-௯౦-౯೦-೯൦-൯෦-෯๐-๙໐-໙༠-༩၀-၉႐-႙០-៩᠐-᠙᥆-᥏᧐-᧙᪀-᪉᪐-᪙᭐-᭙᮰-᮹᱀-᱉᱐-᱙꘠-꘩꣐-꣙꤀-꤉꧐-꧙꧰-꧹꩐-꩙꯰-꯹０-９𐒠-𐒩𐴰-𐴹𑁦-𑁯𑃰-𑃹𑄶-𑄿𑇐-𑇙𑋰-𑋹𑑐-𑑙𑓐-𑓙𑙐-𑙙𑛀-𑛉𑜰-𑜹𑣠-𑣩𑥐-𑥙𑱐-𑱙𑵐-𑵙𑶠-𑶩𑽐-𑽙𖩠-𖩩𖫀-𖫉𖭐-𖭙𝟎-𝟿𞅀-𞅉𞋰-𞋹𞓰-𞓹𞥐-𞥙🯰-🯹]*))\\.[0-9٠-٩۰-۹߀-߉०-९০-৯੦-੯૦-૯୦-୯௦-௯౦-౯೦-೯൦-൯෦-෯๐-๙໐-໙༠-༩၀-၉႐-႙០-៩᠐-᠙᥆-᥏᧐-᧙᪀-᪉᪐-᪙᭐-᭙᮰-᮹᱀-᱉᱐-᱙꘠-꘩꣐-꣙꤀-꤉꧐-꧙꧰-꧹꩐-꩙꯰-꯹０-９𐒠-𐒩𐴰-𐴹𑁦-𑁯𑃰-𑃹𑄶-𑄿𑇐-𑇙𑋰-𑋹𑑐-𑑙𑓐-𑓙𑙐-𑙙𑛀-𑛉𑜰-𑜹𑣠-𑣩𑥐-𑥙𑱐-𑱙𑵐-𑵙𑶠-𑶩𑽐-𑽙𖩠-𖩩𖫀-𖫉𖭐-𖭙𝟎-𝟿𞅀-𞅉𞋰-𞋹𞓰-𞓹𞥐-𞥙🯰-🯹]*(?:[Ee][\\+\\-]?[0-9٠-٩۰-۹߀-߉०-९০-৯੦-੯૦-૯୦-୯௦-௯౦-౯೦-೯൦-൯෦-෯๐-๙໐-໙༠-༩၀-၉႐-႙០-៩᠐-᠙᥆-᥏᧐-᧙᪀-᪉᪐-᪙᭐-᭙᮰-᮹᱀-᱉᱐-᱙꘠-꘩꣐-꣙꤀-꤉꧐-꧙꧰-꧹꩐-꩙꯰-꯹０-９𐒠-𐒩𐴰-𐴹𑁦-𑁯𑃰-𑃹𑄶-𑄿𑇐-𑇙𑋰-𑋹𑑐-𑑙𑓐-𑓙𑙐-𑙙𑛀-𑛉𑜰-𑜹𑣠-𑣩𑥐-𑥙𑱐-𑱙𑵐-𑵙𑶠-𑶩𑽐-𑽙𖩠-𖩩𖫀-𖫉𖭐-𖭙𝟎-𝟿𞅀-𞅉𞋰-𞋹𞓰-𞓹𞥐-𞥙🯰-🯹]+)?)", false),
            ("(?:0[0-9٠-٩۰-۹߀-߉०-९০-৯੦-੯૦-૯୦-୯௦-௯౦-౯೦-೯൦-൯෦-෯๐-๙໐-໙༠-༩၀-၉႐-႙០-៩᠐-᠙᥆-᥏᧐-᧙᪀-᪉᪐-᪙᭐-᭙᮰-᮹᱀-᱉᱐-᱙꘠-꘩꣐-꣙꤀-꤉꧐-꧙꧰-꧹꩐-꩙꯰-꯹０-９𐒠-𐒩𐴰-𐴹𑁦-𑁯𑃰-𑃹𑄶-𑄿𑇐-𑇙𑋰-𑋹𑑐-𑑙𑓐-𑓙𑙐-𑙙𑛀-𑛉𑜰-𑜹𑣠-𑣩𑥐-𑥙𑱐-𑱙𑵐-𑵙𑶠-𑶩𑽐-𑽙𖩠-𖩩𖫀-𖫉𖭐-𖭙𝟎-𝟿𞅀-𞅉𞋰-𞋹𞓰-𞓹𞥐-𞥙🯰-🯹])", false),
            ("!", false),
            ("(?:!=)", false),
            ("(?:\\#\\.\\*)", false),
            ("%", false),
            ("\\&", false),
            ("(?:\\&\\&)", false),
            ("\\(", false),
            ("\\)", false),
            ("\\*", false),
            ("(?:\\*\\*)", false),
            ("\\+", false),
            ("\\-", false),
            ("(?:\\->)", false),
            ("/", false),
            ("(?://)", false),
            (";", false),
            ("<", false),
            ("(?:<<)", false),
            ("(?:<=)", false),
            ("=", false),
            ("(?:==)", false),
            (">", false),
            ("(?:>=)", false),
            ("(?:>>)", false),
            ("(?:Numbers can`t have leading zeros)", false),
            ("\\^", false),
            ("(?:let)", false),
            ("\\|", false),
            ("(?:\\|\\|)", false),
            (r"\s+", true),
        ];
        __lalrpop_util::lexer::MatcherBuilder::new(__strs.iter().copied()).unwrap()
    }
}
pub(crate) use self::__lalrpop_util::lexer::Token;

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action0<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, alloc::vec::Vec<ast::Tld>, usize),
) -> alloc::vec::Vec<ast::Tld>
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action1<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, &'input str, usize),
) -> &'input str
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action2<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, &'input str, usize),
) -> String
{
    String::from(__0)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action3<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, &'input str, usize),
) -> String
{
    String::from(__0)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action4<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, &'input str, usize),
    (_, __1, _): (usize, &'input str, usize),
) -> String
{
    "0".to_string()
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action5<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, &'input str, usize),
) -> String
{
    String::from(__0)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action6<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, (String, Span), usize),
) -> Expr
{
    match __0.0.as_str() {
        "false" | "true" => ctx.expr_literal(ast::LiteralKind::Bool, __0.0, __0.1),
        _ => ctx.expr_var(__0.0, __0.1)
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action7<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, (String, Span), usize),
) -> Expr
{
    ctx.expr_literal(ast::LiteralKind::Float, __0.0, __0.1)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action8<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, (String, Span), usize),
) -> Expr
{
    ctx.expr_literal(ast::LiteralKind::Int, __0.0, __0.1)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action9<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, Expr, usize),
) -> Expr
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action10<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, __0, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
) -> Expr
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action11<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, Expr, usize),
) -> Expr
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action12<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, (Expr, Expr, Span), usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::Call, __0.0, __0.1, __0.2)
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action13<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, Expr, usize),
) -> Expr
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action14<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, x, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.unary(ast::UnOp::Not, x, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action15<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, Expr, usize),
) -> Expr
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action16<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::Pow, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action17<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, Expr, usize),
) -> Expr
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action18<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::Mul, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action19<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::Div, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action20<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::FloorDiv, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action21<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::Rem, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action22<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, Expr, usize),
) -> Expr
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action23<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::Add, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action24<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::Sub, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action25<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, Expr, usize),
) -> Expr
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action26<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::Shl, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action27<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::Shr, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action28<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, Expr, usize),
) -> Expr
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action29<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::BitAnd, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action30<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, Expr, usize),
) -> Expr
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action31<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::BitXor, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action32<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, Expr, usize),
) -> Expr
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action33<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::BitOr, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action34<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, Expr, usize),
) -> Expr
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action35<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::Eq, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action36<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::Ne, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action37<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::Lt, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action38<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::Gt, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action39<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::Le, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action40<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::Ge, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action41<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, Expr, usize),
) -> Expr
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action42<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::And, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action43<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, Expr, usize),
) -> Expr
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action44<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.binary(ast::BinOp::Or, a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action45<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, Expr, usize),
) -> Expr
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action46<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, a, _): (usize, Pattern, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.lambda(a, b, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action47<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, Expr, usize),
) -> Expr
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action48<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, a, _): (usize, Pattern, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, b, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, c, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> Expr
{
    {
        ctx.letrec(a, b, c, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action49<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, Expr, usize),
) -> Expr
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action50<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, (String, Span), usize),
) -> Pattern
{
    ctx.pattern_var(__0.0, __0.1)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action51<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, pat, _): (usize, Pattern, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, body, _): (usize, Expr, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, r, _): (usize, usize, usize),
) -> ast::Tld
{
    {
    	ctx.tld(pat, body, (l, r))
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action52<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, alloc::vec::Vec<ast::Tld>, usize),
) -> alloc::vec::Vec<ast::Tld>
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action53<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __lookbehind: &usize,
    __lookahead: &usize,
) -> alloc::vec::Vec<ast::Tld>
{
    alloc::vec![]
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action54<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, v, _): (usize, alloc::vec::Vec<ast::Tld>, usize),
) -> alloc::vec::Vec<ast::Tld>
{
    v
}

#[allow(unused_variables)]
fn __action55<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __lookbehind: &usize,
    __lookahead: &usize,
) -> usize
{
    *__lookbehind
}

#[allow(unused_variables)]
fn __action56<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __lookbehind: &usize,
    __lookahead: &usize,
) -> usize
{
    *__lookahead
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action57<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, val, _): (usize, Expr, usize),
    (_, val2, _): (usize, Expr, usize),
    (_, r, _): (usize, usize, usize),
) -> (Expr, Expr, Span)
{
    (val, val2, ctx.span(l, r))
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action58<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, val, _): (usize, String, usize),
    (_, r, _): (usize, usize, usize),
) -> (String, Span)
{
    (val, ctx.span(l, r))
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action59<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, val, _): (usize, String, usize),
    (_, r, _): (usize, usize, usize),
) -> (String, Span)
{
    (val, ctx.span(l, r))
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action60<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, l, _): (usize, usize, usize),
    (_, val, _): (usize, String, usize),
    (_, r, _): (usize, usize, usize),
) -> (String, Span)
{
    (val, ctx.span(l, r))
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action61<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, __0, _): (usize, ast::Tld, usize),
) -> alloc::vec::Vec<ast::Tld>
{
    alloc::vec![__0]
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action62<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    (_, v, _): (usize, alloc::vec::Vec<ast::Tld>, usize),
    (_, e, _): (usize, ast::Tld, usize),
) -> alloc::vec::Vec<ast::Tld>
{
    { let mut v = v; v.push(e); v }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action63<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, &'input str, usize),
    __1: (usize, Pattern, usize),
    __2: (usize, &'input str, usize),
    __3: (usize, Expr, usize),
    __4: (usize, &'input str, usize),
    __5: (usize, Expr, usize),
    __6: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action48(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
        __4,
        __5,
        __6,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action64<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action35(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action65<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action36(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action66<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action37(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action67<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action38(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action68<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action39(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action69<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action40(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action70<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action42(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action71<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action44(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action72<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Pattern, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action46(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action73<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, &'input str, usize),
    __1: (usize, Expr, usize),
    __2: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action14(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action74<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action16(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action75<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action18(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action76<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action19(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action77<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action20(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action78<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action79<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action23(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action80<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action24(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action81<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action26(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action82<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action27(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action83<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action29(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action84<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action31(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action85<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, usize, usize),
) -> Expr
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action33(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action86<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, Expr, usize),
    __2: (usize, usize, usize),
) -> (Expr, Expr, Span)
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action57(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action87<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, String, usize),
    __1: (usize, usize, usize),
) -> (String, Span)
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action59(
        ctx,
        input,
        __temp0,
        __0,
        __1,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action88<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, String, usize),
    __1: (usize, usize, usize),
) -> (String, Span)
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action60(
        ctx,
        input,
        __temp0,
        __0,
        __1,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action89<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, String, usize),
    __1: (usize, usize, usize),
) -> (String, Span)
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action58(
        ctx,
        input,
        __temp0,
        __0,
        __1,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action90<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Pattern, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, &'input str, usize),
    __4: (usize, usize, usize),
) -> ast::Tld
{
    let __start0 = __0.0;
    let __end0 = __0.0;
    let __temp0 = __action56(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action51(
        ctx,
        input,
        __temp0,
        __0,
        __1,
        __2,
        __3,
        __4,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action91<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, &'input str, usize),
    __1: (usize, Pattern, usize),
    __2: (usize, &'input str, usize),
    __3: (usize, Expr, usize),
    __4: (usize, &'input str, usize),
    __5: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __5.2;
    let __end0 = __5.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action63(
        ctx,
        input,
        __0,
        __1,
        __2,
        __3,
        __4,
        __5,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action92<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action64(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action93<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action65(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action94<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action66(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action95<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action67(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action96<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action68(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action97<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action69(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action98<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action70(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action99<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action71(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action100<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Pattern, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action72(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action101<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, &'input str, usize),
    __1: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __1.2;
    let __end0 = __1.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action73(
        ctx,
        input,
        __0,
        __1,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action102<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action74(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action103<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action75(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action104<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action76(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action105<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action77(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action106<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action78(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action107<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action79(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action108<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action80(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action109<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action81(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action110<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action82(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action111<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action83(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action112<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action84(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action113<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
) -> Expr
{
    let __start0 = __2.2;
    let __end0 = __2.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action85(
        ctx,
        input,
        __0,
        __1,
        __2,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action114<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Expr, usize),
    __1: (usize, Expr, usize),
) -> (Expr, Expr, Span)
{
    let __start0 = __1.2;
    let __end0 = __1.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action86(
        ctx,
        input,
        __0,
        __1,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action115<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, String, usize),
) -> (String, Span)
{
    let __start0 = __0.2;
    let __end0 = __0.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action87(
        ctx,
        input,
        __0,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action116<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, String, usize),
) -> (String, Span)
{
    let __start0 = __0.2;
    let __end0 = __0.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action88(
        ctx,
        input,
        __0,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action117<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, String, usize),
) -> (String, Span)
{
    let __start0 = __0.2;
    let __end0 = __0.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action89(
        ctx,
        input,
        __0,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action118<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, Pattern, usize),
    __1: (usize, &'input str, usize),
    __2: (usize, Expr, usize),
    __3: (usize, &'input str, usize),
) -> ast::Tld
{
    let __start0 = __3.2;
    let __end0 = __3.2;
    let __temp0 = __action55(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action90(
        ctx,
        input,
        __0,
        __1,
        __2,
        __3,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action119<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __lookbehind: &usize,
    __lookahead: &usize,
) -> alloc::vec::Vec<ast::Tld>
{
    let __start0 = *__lookbehind;
    let __end0 = *__lookahead;
    let __temp0 = __action53(
        ctx,
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action52(
        ctx,
        input,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action120<
    'input,
>(
    ctx: &mut crate::ParseCtx,
    input: &'input str,
    __0: (usize, alloc::vec::Vec<ast::Tld>, usize),
) -> alloc::vec::Vec<ast::Tld>
{
    let __start0 = __0.0;
    let __end0 = __0.2;
    let __temp0 = __action54(
        ctx,
        input,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action52(
        ctx,
        input,
        __temp0,
    )
}
#[allow(clippy::type_complexity, dead_code)]

pub  trait __ToTriple<'input, >
{
    fn to_triple(value: Self) -> Result<(usize,Token<'input>,usize), __lalrpop_util::ParseError<usize, Token<'input>, &'static str>>;
}

impl<'input, > __ToTriple<'input, > for (usize, Token<'input>, usize)
{
    fn to_triple(value: Self) -> Result<(usize,Token<'input>,usize), __lalrpop_util::ParseError<usize, Token<'input>, &'static str>> {
        Ok(value)
    }
}
impl<'input, > __ToTriple<'input, > for Result<(usize, Token<'input>, usize), &'static str>
{
    fn to_triple(value: Self) -> Result<(usize,Token<'input>,usize), __lalrpop_util::ParseError<usize, Token<'input>, &'static str>> {
        match value {
            Ok(v) => Ok(v),
            Err(error) => Err(__lalrpop_util::ParseError::User { error }),
        }
    }
}
