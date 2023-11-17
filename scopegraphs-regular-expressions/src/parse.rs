use crate::regex::Symbol;
use crate::Regex;
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
                Ok(Self::Empty)
            } else {
                Err(syn::Error::new(
                    val.span(),
                    "expected '0', 'e' or symbol here",
                ))
            };
        }

        let name: Path = input.parse()?;
        if name.is_ident("e") {
            Ok(Self::Epsilon)
        } else {
            Ok(Self::Symbol(Symbol { name }))
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
            return Ok(Self::Complement(Box::new(Self::parse_closure(input)?)));
        }

        let inner = Self::parse_atom(input)?;
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![?]) {
            let _ = input.parse::<Token![?]>();
            Ok(Self::Or(Box::new(inner), Box::new(Self::Epsilon)))
        } else if lookahead.peek(Token![*]) {
            let _ = input.parse::<Token![*]>();
            Ok(Self::Repeat(Box::new(inner)))
        } else if lookahead.peek(Token![+]) {
            let _ = input.parse::<Token![+]>();
            Ok(Self::Concat(Box::new(inner.clone()), Box::new(inner)))
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
                Box::new(lhs),
                Box::new(Self::parse_concatenation(input)?),
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
                lhs = Self::Or(Box::new(lhs), Box::new(Self::parse_concatenation(input)?))
            } else if input.peek(Token![&]) {
                let _ = input.parse::<Token![&]>();
                lhs = Self::And(Box::new(lhs), Box::new(Self::parse_concatenation(input)?))
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
    #[test]
    fn test_regex() {
        assert_eq!(parse_regex("A").unwrap(), Symbol("A".into()));
        assert_eq!(
            parse_regex("A | B").unwrap(),
            Or(Box::new(Symbol("A".into())), Box::new(Symbol("B".into())))
        );
        assert_eq!(
            parse_regex("A | B | C").unwrap(),
            Or(
                Box::new(Or(
                    Box::new(Symbol("A".into())),
                    Box::new(Symbol("B".into()))
                )),
                Box::new(Symbol("C".into()))
            )
        );
        assert_eq!(
            parse_regex("A & B").unwrap(),
            And(Box::new(Symbol("A".into())), Box::new(Symbol("B".into())))
        );
        assert_eq!(
            parse_regex("A* B*").unwrap(),
            Concat(
                Box::new(Repeat(Box::new(Symbol("A".into())))),
                Box::new(Repeat(Box::new(Symbol("B".into()))))
            )
        );
        assert_eq!(
            parse_regex("~A ~B").unwrap(),
            Concat(
                Box::new(Complement(Box::new(Symbol("A".into())))),
                Box::new(Complement(Box::new(Symbol("B".into()))))
            )
        );
        assert_eq!(parse_regex("e").unwrap(), Epsilon);
        assert_eq!(parse_regex("0").unwrap(), Empty);
    }
}
