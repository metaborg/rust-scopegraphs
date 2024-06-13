use winnow::ascii::multispace0;
use winnow::combinator::{alt, delimited, opt, preceded, repeat, separated, terminated};
use winnow::error::{ParserError, StrContext};

use crate::ast::{Expr, Program, RecordDef, Type};
use winnow::prelude::*;
use winnow::seq;
use winnow::stream::AsChar;
use winnow::token::{one_of, take_while};

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
        .verify(|i: &str| !["in", "new", "letrec", "record", "let"].contains(&i)))
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
    .parse_next(input)
    .map(|i| i.parse().expect("not an integer"))
}

fn parse_type(input: &mut &'_ str) -> PResult<Type> {
    ws(alt((
        "int".value(Type::Int),
        parse_ident.map(Type::StructRef),
    )))
    .parse_next(input)
}

fn parse_field_def(input: &mut &'_ str) -> PResult<(String, Type)> {
    seq!(
        _: multispace0,
        parse_ident,
        _: ws(":"),
        parse_type,
        _: multispace0,
    )
    .parse_next(input)
}

fn parse_field_defs(input: &mut &'_ str) -> PResult<Vec<(String, Type)>> {
    terminated(separated(0.., ws(parse_field_def), ws(",")), opt(ws(","))).parse_next(input)
}

fn parse_field(input: &mut &'_ str) -> PResult<(String, Expr)> {
    seq!(
        _: multispace0,
        parse_ident,
        _: ws(":"),
        parse_expr,
        _: multispace0,
    )
    .parse_next(input)
}

fn parse_fields(input: &mut &'_ str) -> PResult<Vec<(String, Expr)>> {
    terminated(separated(0.., ws(parse_field), ws(",")), opt(ws(","))).parse_next(input)
}

fn parse_item(input: &mut &'_ str) -> PResult<RecordDef> {
    seq! {RecordDef {
        name: parse_ident,
        // `_` fields are ignored when building the record
        _: ws("{"),
        fields: parse_field_defs,
        _: ws("}"),
    }}
    .parse_next(input)
}

fn parse_value(input: &mut &'_ str) -> PResult<(String, Expr)> {
    seq!(
        parse_ident,
        _: ws("="),
        parse_expr,
        _: ws(";")
    )
    .parse_next(input)
}

fn parse_values(input: &mut &'_ str) -> PResult<Vec<(String, Expr)>> {
    repeat(0.., parse_value).parse_next(input)
}

fn parse_basic_expr(input: &mut &'_ str) -> PResult<Expr> {
    alt((
        parse_int.map(Expr::Number),
        parse_ident.map(Expr::Ident),
        seq! {
            _: ws("new"),
            parse_ident,
            // `_` fields are ignored when building the record
            _: ws("{"),
            parse_fields,
            _: ws("}"),
        }
        .map(|(name, fields)| Expr::StructInit { name, fields }),
        seq! {
            _: ws("letrec"),
            parse_values,
            _: ws("in"),
            parse_expr,
        }
        .map(|(values, in_expr)| Expr::LetRec {
            values,
            in_expr: Box::new(in_expr),
        }),
        seq! {
            _: ws("let"),
            parse_value,
            _: ws("in"),
            parse_expr,
        }
        .map(|((name, value), in_expr)| Expr::Let {
            name,
            value: Box::new(value),
            in_expr: Box::new(in_expr),
        }),
        seq! {
            _: ws("("),
            parse_expr,
            _: ws(")"),
        }
        .map(|(i,)| i),
    ))
    .context(StrContext::Label("parse expr"))
    .parse_next(input)
}

fn parse_expr(input: &mut &'_ str) -> PResult<Expr> {
    let first = ws(parse_basic_expr).parse_next(input)?;
    let mut res = repeat(0.., (ws("."), parse_ident).map(|(_, i)| i)).fold(
        || first.clone(),
        |acc, val| Expr::FieldAccess(Box::new(acc), val),
    );

    res.parse_next(input)
}

enum ItemOrExpr {
    Item(RecordDef),
    Expr(Expr),
}

pub fn parse(mut input: &str) -> PResult<Program> {
    let mut items = Vec::new();
    let mut main = None;

    while !input.is_empty() {
        match ws(alt((
            ws(preceded(ws("record"), parse_item.map(ItemOrExpr::Item))),
            seq!(
                _: ws("main"),
                _: ws("="),
                ws(parse_expr.map(ItemOrExpr::Expr)),
                _: ws(";"),
            )
            .map(|(i,)| i),
        ))
        .context(StrContext::Label("parse item")))
        .parse_next(&mut input)?
        {
            ItemOrExpr::Expr(e) => main = Some(e),
            ItemOrExpr::Item(i) => items.push(i),
        }
    }

    Ok(Program {
        record_types: items,
        main: main.expect("no main"),
    })
}
