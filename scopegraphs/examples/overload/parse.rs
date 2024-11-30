use winnow::ascii::multispace0;
use winnow::combinator::{
    alt, cut_err, delimited, eof, fail, opt, preceded, repeat, separated, terminated, trace,
};
use winnow::error::{ErrMode, ParserError, StrContext};

use crate::ast::{Arg, Expr, Function, Program, Type};
use winnow::prelude::*;
use winnow::seq;
use winnow::stream::AsChar;
use winnow::token::{one_of, take_while};

const KEYWORDS: [&str; 7] = ["fun", "if", "else", "true", "false", "int", "bool"];

fn ws<'a, F, O, E: ParserError<&'a str>>(inner: F) -> impl Parser<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

fn parse_ident(input: &mut &'_ str) -> PResult<String> {
    ws((
        one_of(|c: char| c.is_alpha() || c == '_'),
        take_while(0.., |c: char| c.is_alphanum() || c == '_'),
    )
        .recognize()
        .verify(|i: &str| !KEYWORDS.contains(&i)))
    .context(StrContext::Label("parse ident"))
    .parse_next(input)
    .map(|i| i.to_string())
}

fn parse_int(input: &mut &'_ str) -> PResult<u64> {
    repeat(
        1..,
        terminated(one_of('0'..='9'), repeat(0.., '_').map(|()| ())),
    )
    .map(|()| ())
    .recognize()
    .context(StrContext::Label("parse int"))
    .parse_next(input)
    .map(|i| i.parse().expect("not an integer"))
}

fn parse_bool(input: &mut &'_ str) -> PResult<bool> {
    ws(alt(("true".value(true), "false".value(false))))
        .context(StrContext::Label("parse boolean literal"))
        .parse_next(input)
}

fn parse_type(input: &mut &'_ str) -> PResult<Type> {
    ws(alt(("int".value(Type::IntT), "bool".value(Type::BoolT))))
        .context(StrContext::Label("parse type literal"))
        .parse_next(input)
}

fn parse_type_anno(input: &mut &'_ str) -> PResult<Type> {
    preceded(ws(":"), parse_type)
        .context(StrContext::Label("parse type annotation"))
        .parse_next(input)
}

fn parse_base_expr(input: &mut &'_ str) -> PResult<Expr> {
    alt((
        parse_int.map(Expr::IntLit),
        parse_bool.map(Expr::BoolLit),
        seq! {
            _: ws("if"),
            parse_expr,
            _: ws("{"),
            parse_expr,
            _: ws("}"),
            _: ws("else"),
            _: ws("{"),
            parse_expr,
            _: ws("}"),
        }
        .map(|(c, if_branch, else_branch)| {
            Expr::IfThenElse(Box::new(c), Box::new(if_branch), Box::new(else_branch))
        }),
        seq! {
            parse_ident,
            _: ws("("),
            parse_exprs,
            _: ws(")")
        }
        .map(|(function_name, args)| Expr::FunCall(function_name, args)),
        parse_ident.map(Expr::Ident), // must come after funcall
        parse_bracketed,
    ))
    .context(StrContext::Label("parse base expr"))
    .parse_next(input)
}

fn parse_arith_expr(input: &mut &'_ str) -> PResult<Expr> {
    separated(1.., parse_base_expr, ws("+"))
        .context(StrContext::Label("parse arith expression"))
        .parse_next(input)
        .map(|operands: Vec<Expr>| {
            operands
                .into_iter()
                .reduce(|acc, op| Expr::Plus(acc.into(), op.into()))
                .expect("at least one occurrence, so unwrapping is safe!")
        })
}

fn parse_cmp_expr(input: &mut &'_ str) -> PResult<Expr> {
    separated(1..3, parse_arith_expr, ws("<"))
        .context(StrContext::Label("parse comparison expression"))
        .parse_next(input)
        .map(|operands: Vec<Expr>| {
            operands
                .into_iter()
                .reduce(|acc, op| Expr::Lt(acc.into(), op.into()))
                .expect("at least one occurrence, so unwrapping is safe!")
        })
}

fn parse_bracketed(input: &mut &'_ str) -> PResult<Expr> {
    delimited(ws("("), parse_expr, ws(")"))
        .context(StrContext::Label("parse bracketed expression"))
        .parse_next(input)
}

fn parse_expr(input: &mut &'_ str) -> PResult<Expr> {
    seq! {
        parse_cmp_expr,
        opt(preceded(ws(":"), parse_type)),
    }
    .map(|(expr, type_opt)| {
        let expr_clone = expr.clone();
        type_opt
            .map(|t| Expr::Ascribe(expr_clone.into(), t))
            .unwrap_or(expr)
    })
    .context(StrContext::Label("parse expression"))
    .parse_next(input)
}

fn parse_exprs(input: &mut &'_ str) -> PResult<Vec<Expr>> {
    terminated(separated(0.., ws(parse_expr), ws(",")), opt(ws(",")))
        .context(StrContext::Label("parse expression list"))
        .parse_next(input)
}

fn parse_arg(input: &mut &'_ str) -> PResult<Arg> {
    seq! {
        parse_ident,
        opt(parse_type_anno)
    }
    .context(StrContext::Label("parse argument"))
    .map(|(name, type_ann)| Arg { name, type_ann })
    .parse_next(input)
}

fn parse_args(input: &mut &'_ str) -> PResult<Vec<Arg>> {
    terminated(separated(0.., ws(parse_arg), ws(",")), opt(ws(",")))
        .context(StrContext::Label("parse argument list"))
        .parse_next(input)
}

fn parse_function(input: &mut &'_ str) -> PResult<Function> {
    seq!(
        _: ws("fun"),
        parse_ident,
        delimited(ws("("), parse_args, ws(")")),
        opt(parse_type_anno),
        _: ws("="),
        parse_expr,
        _: ws(";"),
    )
    .map(|(name, args, return_type, body)| Function {
        name,
        args,
        return_type,
        body,
    })
    .context(StrContext::Label("parse function"))
    .parse_next(input)
}

pub fn parse_program(input: &mut &'_ str) -> PResult<Program> {
    seq! {
        repeat(0.., parse_function),
        _: ws("$"),
        parse_expr,
    }
    .map(|(functions, main)| Program { functions, main })
    .context(StrContext::Label("parse program"))
    .parse_next(input)
}

pub fn parse(mut input: &str) -> PResult<Program> {
    terminated(ws(parse_program), eof).parse_next(&mut input)
}

pub fn parse_trace(mut input: &str) -> PResult<Program> {
    trace("trace program", parse_program).parse_next(&mut input)
}

#[cfg(test)]
mod test {
    use std::thread;

    use winnow::{
        combinator::{terminated, trace},
        Parser,
    };

    use crate::{
        parse::{parse, parse_arith_expr, parse_base_expr, parse_cmp_expr, parse_ident},
        Expr, Type,
    };

    use super::{parse_args, parse_expr, parse_function, parse_trace};

    #[test]
    pub fn parse_expr_true() {
        let mut input = "true";
        assert_eq!(Expr::BoolLit(true), parse_expr(&mut input).unwrap());
    }

    #[test]
    pub fn parse_expr_false() {
        let mut input = "false";
        assert_eq!(Expr::BoolLit(false), parse_expr(&mut input).unwrap());
    }

    #[test]
    pub fn parse_lit_zero() {
        let mut input = "0";
        assert_eq!(Expr::IntLit(0), parse_expr(&mut input).unwrap());
    }

    #[test]
    pub fn parse_lit_42() {
        let mut input = "42";
        assert_eq!(Expr::IntLit(42), parse_expr(&mut input).unwrap());
    }

    #[test]
    pub fn parse_ident_x() {
        let mut input = "x";
        assert_eq!(Expr::Ident("x".into()), parse_expr(&mut input).unwrap());
    }

    #[test]
    pub fn parse_ident_x1n() {
        let mut input = "x1";
        assert_eq!(Expr::Ident("x1".into()), parse_expr(&mut input).unwrap());
    }

    #[test]
    pub fn parse_ident_usc_x() {
        let mut input = "_x";
        assert_eq!(Expr::Ident("_x".into()), parse_expr(&mut input).unwrap());
    }

    #[test]
    pub fn parse_ident_empty() {
        let mut input = "";
        assert!(parse_base_expr(&mut input).is_err());
    }

    #[test]
    pub fn parse_plus() {
        let mut input = "x + y";
        assert_eq!(
            Expr::Plus(
                Expr::Ident("x".into()).into(),
                Expr::Ident("y".into()).into(),
            ),
            trace("parse expr", parse_expr)
                .parse_next(&mut input)
                .unwrap(),
        );
    }

    #[test]
    pub fn parse_plus_assoc() {
        let mut input = "x + 2 + y";
        assert_eq!(
            Expr::Plus(
                Box::new(Expr::Plus(
                    Expr::Ident("x".into()).into(),
                    Expr::IntLit(2).into(),
                )),
                Box::new(Expr::Ident("y".into()).into()),
            ),
            trace("parse expr", parse_expr)
                .parse_next(&mut input)
                .unwrap(),
        );
    }

    #[test]
    pub fn parse_lt() {
        let mut input = "x < 42";
        assert_eq!(
            Expr::Lt(
                Box::new(Expr::Ident("x".into())),
                Box::new(Expr::IntLit(42)),
            ),
            trace("parse expr", parse_expr)
                .parse_next(&mut input)
                .unwrap(),
        );
    }

    #[test]
    pub fn parse_lt_non_assoc() {
        let mut input = "x < 42 < y";
        assert_eq!(
            Expr::Lt(
                Box::new(Expr::Ident("x".into())),
                Box::new(Expr::IntLit(42)),
            ),
            trace("parse expr", parse_expr)
                .parse_next(&mut input)
                .unwrap(),
        );
    }

    #[test]
    pub fn parse_lt_non_assoc_bracketed() {
        let mut input = "(x < 42 < y)";
        assert!(trace("parse expr", parse_expr)
            .parse_next(&mut input)
            .is_err());
    }

    #[test]
    pub fn parse_funcall_no_args() {
        let mut input = "foo()";
        assert_eq!(
            Expr::FunCall("foo".into(), vec![],),
            trace("parse expr", parse_expr)
                .parse_next(&mut input)
                .unwrap(),
        );
    }

    #[test]
    pub fn parse_if_in_if() {
        let mut input = "if a { if b < f(42, false) { c + 2 } else { d(true) } } else { f : int }";
        assert_eq!(
            Expr::IfThenElse(
                Expr::Ident("a".into()).into(),
                Expr::IfThenElse(
                    Expr::Lt(
                        Expr::Ident("b".into()).into(),
                        Expr::FunCall("f".into(), vec![Expr::IntLit(42), Expr::BoolLit(false),])
                            .into(),
                    )
                    .into(),
                    Expr::Plus(Expr::Ident("c".into()).into(), Expr::IntLit(2).into(),).into(),
                    Expr::FunCall("d".into(), vec![Expr::BoolLit(true),]).into(),
                )
                .into(),
                Expr::Ascribe(Expr::Ident("f".into()).into(), Type::IntT,).into(),
            ),
            parse_expr(&mut input).unwrap()
        );
    }

    #[test]
    pub fn parse_all_constructs() {
        let program = "
            fun tt() = true;
            fun not(b) = if b { false } else { true };
            fun and(b1: bool, b2): bool = 
                if b1 {
                    if b2 {
                        true
                    } else {
                        false
                    }
                } else {
                    false
                };

            $ and(not(false), tt())
        ";

        assert!(parse_trace(program).is_ok());
    }
}
