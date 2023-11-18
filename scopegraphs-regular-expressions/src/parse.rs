use crate::regex::Symbol;
use crate::Regex;
use std::rc::Rc;
use syn::parse::{Parse, ParseStream};
use syn::{parenthesized, Path, Token};

impl Regex {
    /// A symbol is an element from the alphabet, usually an enum variant.
    /// In the current implementation, that's what we assume, essentially
    /// parsing a path.
    ///
    /// TODO: maybe also parse integers or other kinds of labels
    fn parse_symbol(input: ParseStream) -> syn::Result<Self> {
        if input.peek(syn::LitInt) {
            let val = input.parse::<syn::LitInt>()?;
            return if let Ok(0) = val.base10_parse() {
                Ok(Self::EmptySet)
            } else {
                Err(syn::Error::new(
                    val.span(),
                    "expected '0', 'e' or symbol here",
                ))
            };
        }

        let name: Path = input.parse()?;
        if name.is_ident("e") {
            Ok(Self::EmptyString)
        } else {
            Ok(Self::Symbol(Symbol { name }.into()))
        }
    }

    /// Parse either a symbol or a parenthesized expression
    fn parse_atom(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::token::Paren) {
            let inner;
            parenthesized!(inner in input);
            Self::parse_regex(&inner)
        } else if input.peek(syn::Ident) | input.peek(syn::LitInt) {
            Self::parse_symbol(input)
        } else {
            Err(lookahead.error())
        }
    }

    fn parse_closure(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![~]) {
            let _ = input.parse::<Token![~]>()?;
            return Ok(Self::Complement(Rc::new(Self::parse_closure(input)?)));
        }

        let inner = Self::parse_atom(input)?;
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![?]) {
            let _ = input.parse::<Token![?]>();
            Ok(Self::Or(Rc::new(inner), Rc::new(Self::EmptyString)))
        } else if lookahead.peek(Token![*]) {
            let _ = input.parse::<Token![*]>();
            Ok(Self::Repeat(Rc::new(inner)))
        } else if lookahead.peek(Token![+]) {
            let _ = input.parse::<Token![+]>();
            Ok(Self::Concat(Rc::new(inner.clone()), Rc::new(inner)))
        } else {
            Ok(inner)
        }
    }

    fn parse_concatenation(input: ParseStream) -> syn::Result<Self> {
        let lhs = Self::parse_closure(input)?;
        if input.peek(syn::Ident)
            | input.peek(syn::token::Paren)
            | input.peek(syn::LitInt)
            | input.peek(Token![~])
        {
            Ok(Self::Concat(
                Rc::new(lhs),
                Rc::new(Self::parse_concatenation(input)?),
            ))
        } else {
            Ok(lhs)
        }
    }

    fn parse_regex(input: ParseStream) -> syn::Result<Self> {
        let mut lhs = Self::parse_concatenation(input)?;

        loop {
            if input.is_empty() {
                return Ok(lhs);
            } else if input.peek(Token![|]) {
                let _ = input.parse::<Token![|]>();
                lhs = Self::Or(Rc::new(lhs), Rc::new(Self::parse_concatenation(input)?))
            } else if input.peek(Token![&]) {
                let _ = input.parse::<Token![&]>();
                lhs = Self::And(Rc::new(lhs), Rc::new(Self::parse_concatenation(input)?))
            } else {
                return Err(syn::Error::new(
                    input.span(),
                    "expected '|', '&' or the end of the regular expression here",
                ));
            }
        }
    }
}

impl Parse for Regex {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Self::parse_regex(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::{parse_regex, Regex::*};
    use std::rc::Rc;

    #[test]
    fn test_regex() {
        assert_eq!(parse_regex("A").unwrap(), Symbol(Rc::new("A".into())));
        assert_eq!(
            parse_regex("A | B").unwrap(),
            Or(
                Rc::new(Symbol(Rc::new("A".into()))),
                Rc::new(Symbol(Rc::new("B".into())))
            )
        );
        assert_eq!(
            parse_regex("A | B | C").unwrap(),
            Or(
                Rc::new(Or(
                    Rc::new(Symbol(Rc::new("A".into()))),
                    Rc::new(Symbol(Rc::new("B".into())))
                )),
                Rc::new(Symbol(Rc::new("C".into())))
            )
        );
        assert_eq!(
            parse_regex("A & B").unwrap(),
            And(
                Rc::new(Symbol(Rc::new("A".into()))),
                Rc::new(Symbol(Rc::new("B".into())))
            )
        );
        assert_eq!(
            parse_regex("A* B*").unwrap(),
            Concat(
                Rc::new(Repeat(Rc::new(Symbol(Rc::new("A".into()))))),
                Rc::new(Repeat(Rc::new(Symbol(Rc::new("B".into())))))
            )
        );
        assert_eq!(
            parse_regex("~A ~B").unwrap(),
            Concat(
                Rc::new(Complement(Rc::new(Symbol(Rc::new("A".into()))))),
                Rc::new(Complement(Rc::new(Symbol(Rc::new("B".into())))))
            )
        );
        assert_eq!(parse_regex("0").unwrap(), EmptySet);
        assert_eq!(parse_regex("e").unwrap(), EmptyString);
    }
}
