use crate::regex::Symbol;
use crate::Regex;
use std::fmt::Debug;
use std::ops::Deref;
use std::rc::Rc;
use syn::parse::{Parse, ParseStream};
use syn::{parenthesized, Path, Token};

#[derive(Clone, PartialEq, Eq)]
enum RegexSymbol {
    Zero,
    Epsilon,
    Neg,
    Repeat,
    Plus,
    Optional,
    Or,
    And,
    Regex(Rc<Regex>), // used for labels and parenthesized expressions
    End,
}

impl Debug for RegexSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Zero => write!(f, "'0'"),
            Self::Epsilon => write!(f, "'e'"),
            Self::Neg => write!(f, "'~'"),
            Self::Repeat => write!(f, "'*'"),
            Self::Plus => write!(f, "'+'"),
            Self::Optional => write!(f, "'?'"),
            Self::Or => write!(f, "'|'"),
            Self::And => write!(f, "'&'"),
            Self::Regex(regex) => regex.as_ref().fmt(f),
            Self::End => write!(f, "'*'"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum RegexAutomatonState {
    State0,
    State1,
    State2,
    State3,
    State4,
    State5,
    State6,
    State7,
    State8,
    State9,
    State10,
    State11,
    State12,
    State13,
    State14,
}

#[derive(Clone)]
enum StackSymbol {
    State(RegexAutomatonState),
    Regex(Rc<Regex>),
    Symbol(RegexSymbol),
}

impl Debug for StackSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::State(state) => state.fmt(f),
            Self::Regex(regex) => f.write_fmt(format_args!("({:?})", regex.as_ref())),
            Self::Symbol(symbol) => symbol.fmt(f),
        }
    }
}

struct RegexParser<'a> {
    input: ParseStream<'a>,
    state: RegexAutomatonState,
    next: Option<RegexSymbol>,
    symbol_stack: Vec<StackSymbol>,
}

use RegexAutomatonState::*;
use RegexSymbol::*;

impl<'a> RegexParser<'a> {
    fn new(input: ParseStream<'a>) -> Self {
        Self {
            input,
            state: State0,
            next: None,
            symbol_stack: vec![StackSymbol::State(State0)],
        }
    }

    fn scan(&self) -> syn::Result<RegexSymbol> {
        if self.input.is_empty() {
            return Ok(RegexSymbol::End);
        }

        let lookahead = self.input.lookahead1();

        // Rust performs parenthesis matching: leverage that here.
        if lookahead.peek(syn::token::Paren) {
            let inner;
            parenthesized!(inner in self.input);
            return Regex::parse(&inner).map(|re| RegexSymbol::Regex(Rc::new(re)));
        }

        // Scan '0' symbol
        if lookahead.peek(syn::LitInt) {
            let val = self.input.parse::<syn::LitInt>()?;
            if let Ok(0) = val.base10_parse() {
                return Ok(RegexSymbol::Zero);
            }
        }

        // Scan 'e' and names
        if lookahead.peek(syn::Ident) {
            let name = self.input.parse::<Path>()?;
            if name.is_ident("e") {
                return Ok(RegexSymbol::Epsilon);
            } else {
                let regex = Regex::Symbol(Symbol { name }.into());
                return Ok(RegexSymbol::Regex(Rc::new(regex)));
            }
        }

        // Scan '~' symbol
        if lookahead.peek(Token![~]) {
            self.input.parse::<Token![~]>()?;
            return Ok(RegexSymbol::Neg);
        }

        // Scan '*' symbol
        if lookahead.peek(Token![*]) {
            self.input.parse::<Token![*]>()?;
            return Ok(RegexSymbol::Repeat);
        }

        // Scan '+' symbol
        if lookahead.peek(Token![+]) {
            self.input.parse::<Token![+]>()?;
            return Ok(RegexSymbol::Plus);
        }

        // Scan '?' symbol
        if lookahead.peek(Token![?]) {
            self.input.parse::<Token![?]>()?;
            return Ok(RegexSymbol::Optional);
        }

        // Scan '|' symbol
        if lookahead.peek(Token![|]) {
            self.input.parse::<Token![|]>()?;
            return Ok(RegexSymbol::Or);
        }

        // Scan '&' symbol
        if lookahead.peek(Token![&]) {
            self.input.parse::<Token![&]>()?;
            return Ok(RegexSymbol::And);
        }

        Err(lookahead.error())
    }

    fn shift(&mut self, new_state: RegexAutomatonState) -> syn::Result<bool> {
        let next_symbol = self.next()?;
        self.symbol_stack
            .push(StackSymbol::Symbol(next_symbol.clone()));
        self.symbol_stack.push(StackSymbol::State(new_state));
        self.state = new_state;
        self.next = Some(self.scan()?);
        Ok(false) // not in accepting state
    }

    // reduce
    fn top_stack_symbol(&mut self) -> syn::Result<StackSymbol> {
        if let Some(StackSymbol::State(_)) = self.symbol_stack.pop() {
            if let Some(symbol) = self.symbol_stack.pop() {
                Ok(symbol)
            } else {
                self.error("internal parsing error: expected non-empty stack")
            }
        } else {
            self.error("internal parsing error: expected state on top of stack")
        }
    }

    fn top_symbol(&mut self) -> syn::Result<RegexSymbol> {
        if let StackSymbol::Symbol(symbol) = self.top_stack_symbol()? {
            Ok(symbol)
        } else {
            self.error("internal parsing error: expected symbol on top of stack")
        }
    }

    fn top_regex(&mut self) -> syn::Result<Rc<Regex>> {
        if let StackSymbol::Regex(regex) = self.top_stack_symbol()? {
            Ok(regex)
        } else {
            self.error("internal parsing error: expected regex on top of stack")
        }
    }

    // reduce atoms

    fn reduce_atom(&mut self, expected: RegexSymbol, result: Regex) -> syn::Result<bool> {
        if expected == self.top_symbol()? {
            self.goto(Rc::new(result))
        } else {
            self.error("internal parsing error: expected atom on top of stack")
        }
    }

    fn reduce_zero(&mut self) -> syn::Result<bool> {
        self.reduce_atom(RegexSymbol::Zero, Regex::EmptySet)
    }

    fn reduce_epsilon(&mut self) -> syn::Result<bool> {
        self.reduce_atom(RegexSymbol::Epsilon, Regex::EmptyString)
    }

    // Reduces both symbol literals and parenthesized regexes.
    fn reduce_symbol(&mut self) -> syn::Result<bool> {
        match self.top_symbol()? {
            Regex(re) => self.goto(re.clone()),
            _ => self.error("internal parsing error: expected regex on top of stack"),
        }
    }

    // reduce unary operators

    fn reduce_unary_prefix(
        &mut self,
        expected: RegexSymbol,
        build: impl Fn(Rc<Regex>) -> Regex,
    ) -> syn::Result<bool> {
        let re = self.top_regex()?;
        self.reduce_atom(expected, build(re))
    }

    fn reduce_neg(&mut self) -> syn::Result<bool> {
        self.reduce_unary_prefix(RegexSymbol::Neg, Regex::Complement)
    }

    fn reduce_unary_postfix(
        &mut self,
        expected: RegexSymbol,
        build: impl Fn(Rc<Regex>) -> Regex,
    ) -> syn::Result<bool> {
        if expected != self.top_symbol()? {
            return self
                .error("internal parsing error: expected post-fix operator on top of stack");
        }
        let re = self.top_regex()?;
        self.goto(Rc::new(build(re)))
    }

    fn reduce_repeat(&mut self) -> syn::Result<bool> {
        self.reduce_unary_postfix(RegexSymbol::Repeat, Regex::Repeat)
    }

    fn reduce_plus(&mut self) -> syn::Result<bool> {
        self.reduce_unary_postfix(RegexSymbol::Plus, |re| {
            Regex::Concat(re.clone(), Rc::new(Regex::Repeat(re)))
        })
    }

    fn reduce_optional(&mut self) -> syn::Result<bool> {
        self.reduce_unary_postfix(RegexSymbol::Optional, |re| {
            Regex::Or(Rc::new(Regex::EmptyString), re)
        })
    }

    // reduce binary operators

    fn reduce_concat(&mut self) -> syn::Result<bool> {
        // right-hand-side is on top of stack ...
        let r = self.top_regex()?;
        let l = self.top_regex()?;
        self.goto(Rc::new(Regex::Concat(l, r)))
    }

    fn reduce_binary_infix(
        &mut self,
        expected: RegexSymbol,
        build: impl Fn(Rc<Regex>, Rc<Regex>) -> Regex,
    ) -> syn::Result<bool> {
        // right-hand-side is on top of stack ...
        let r = self.top_regex()?;
        if expected != self.top_symbol()? {
            return self.error("internal parsing error: incorrect operator on top level of stack");
        }
        let l = self.top_regex()?;
        self.goto(Rc::new(build(l, r)))
    }

    fn reduce_or(&mut self) -> syn::Result<bool> {
        self.reduce_binary_infix(RegexSymbol::Or, Regex::Or)
    }

    fn reduce_and(&mut self) -> syn::Result<bool> {
        self.reduce_binary_infix(RegexSymbol::And, Regex::And)
    }

    // goto jump table

    fn goto(&mut self, result: Rc<Regex>) -> syn::Result<bool> {
        if self.symbol_stack.is_empty() {
            return self
                .error("internal parsing error: expected non-empty stack on a 'goto' action'");
        }
        if let Some(StackSymbol::State(st)) = self.symbol_stack.last() {
            self.state = match st {
                State0 => State1,
                State1 => State9,
                State5 => State12,
                State9 => State9,
                State10 => State13,
                State11 => State14,
                State12 => State9,
                State13 => State9,
                State14 => State9,
                _ => return self.error(
                    "internal parsing error: cannot perform 'goto' action' on current stack state",
                ),
            };
            self.symbol_stack.push(StackSymbol::Regex(result.clone()));
            self.symbol_stack.push(StackSymbol::State(self.state));
            Ok(false)
        } else {
            self.error("internal parsing error: expected state on top of symbol stack")
        }
    }

    fn accept() -> syn::Result<bool> {
        Ok(true)
    }

    fn build_error(&mut self, msg: &str) -> syn::Error {
        syn::Error::new(self.input.span(), msg)
    }

    fn error<T>(&mut self, msg: &str) -> syn::Result<T> {
        Err(self.build_error(msg))
    }

    fn init(&mut self) -> syn::Result<()> {
        self.next = Some(self.scan()?);
        Ok(())
    }

    fn next(&mut self) -> syn::Result<RegexSymbol> {
        if let Some(next) = self.next.clone() {
            Ok(next)
        } else {
            self.error("internal parsing error: forgot to initialize?")
        }
    }

    fn step(&mut self) -> syn::Result<bool> {
        match self.state {
            State0 | State5 | State10 | State11 => match self.next()? {
                Zero => self.shift(State2),
                Epsilon => self.shift(State3),
                Regex(_) => self.shift(State4),
                Neg => self.shift(State5),
                _ => self.error(
                    "expected '0', 'e', '~', label or parenthesized regular expression here",
                ),
            },
            State1 => match self.next()? {
                Zero => self.shift(State2),
                Epsilon => self.shift(State3),
                Regex(_) => self.shift(State4),
                Neg => self.shift(State5),
                Repeat => self.shift(State6),
                Plus => self.shift(State7),
                Optional => self.shift(State8),
                Or => self.shift(State10),
                And => self.shift(State11),
                End => Self::accept(),
            },
            State2 => self.reduce_zero(),
            State3 => self.reduce_epsilon(),
            State4 => self.reduce_symbol(),
            State6 => self.reduce_repeat(),
            State7 => self.reduce_plus(),
            State8 => self.reduce_optional(),
            State9 => {
                match self.next()? {
                    // concat is right-associative, so shift in case of a new literal/pre-fix operator
                    Zero => self.shift(State2),
                    Epsilon => self.shift(State3),
                    Regex(_) => self.shift(State4),
                    Neg => self.shift(State5),
                    // post-fix operators have priority over concat, so shift here
                    Repeat => self.shift(State6),
                    Plus => self.shift(State7),
                    Optional => self.shift(State8),
                    // concat has priority over '|' and  '&', so reduce here
                    Or => self.reduce_concat(),
                    And => self.reduce_concat(),
                    End => self.reduce_concat(),
                }
            }
            State12 => {
                match self.next()? {
                    // neg has top priority, but is ambiguous with posf-fix operators.
                    Zero => self.reduce_neg(),
                    Epsilon => self.reduce_neg(),
                    Regex(_) => self.reduce_neg(),
                    Neg => self.reduce_neg(),
                    Repeat => self.error(
                        "ambiguous regex: simultaneous use of '~' prefix and '*' postfix operator",
                    ),
                    Plus => self.error(
                        "ambiguous regex: simultaneous use of '~' prefix and '+' postfix operator",
                    ),
                    Optional => self.error(
                        "ambiguous regex: simultaneous use of '~' prefix and '?' postfix operator",
                    ),
                    Or => self.reduce_neg(),
                    And => self.reduce_neg(),
                    End => self.reduce_neg(),
                }
            }
            State13 => {
                match self.next()? {
                    // or has lowest priority, so shift in any case
                    Zero => self.shift(State2),
                    Epsilon => self.shift(State3),
                    Regex(_) => self.shift(State4),
                    Neg => self.shift(State5),
                    Repeat => self.shift(State6),
                    Plus => self.shift(State7),
                    Optional => self.shift(State8),
                    // or is left-associative, so reduce eagerly
                    Or => self.reduce_or(),
                    And => self.shift(State11),
                    End => self.reduce_or(),
                }
            }
            State14 => {
                match self.next()? {
                    // '&' has priority over '|' only, so shift in any other case
                    Zero => self.shift(State2),
                    Epsilon => self.shift(State3),
                    Regex(_) => self.shift(State4),
                    Neg => self.shift(State5),
                    Repeat => self.shift(State6),
                    Plus => self.shift(State7),
                    Optional => self.shift(State8),
                    // has priority over '|'
                    Or => self.reduce_and(),
                    // and is left-recursive, so reduce eagerly
                    And => self.reduce_and(),
                    End => self.reduce_and(),
                }
            }
        }
    }

    fn finalize(mut self) -> syn::Result<Regex> {
        let regex = self.top_regex()?;
        if let Some(StackSymbol::State(State0)) = self.symbol_stack.pop() {
            if self.symbol_stack.is_empty() {
                Ok(regex.deref().clone())
            } else {
                self.error("error: residual input after parsing finished.")
            }
        } else {
            self.error("internal parsing error: expected state 0 on bottom of stack.")
        }
    }

    pub fn parse_regex<'b: 'a>(input: ParseStream<'b>) -> syn::Result<Regex> {
        let mut parser = RegexParser::new(input);
        parser.init()?;
        let mut accept = false;
        while !accept {
            accept = parser.step()?
        }
        parser.finalize()
    }
}

impl Parse for Regex {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        RegexParser::parse_regex(input)
    }
}

#[cfg(test)]
mod tests {
    // Most test cases derived from
    // https://github.com/metaborg/nabl/blob/master/statix.test/syntax/regex.spt

    use crate::{parse_regex, Regex::*};
    use std::rc::Rc;

    #[test]
    fn test_symbols() {
        assert_eq!(parse_regex("A").unwrap(), Symbol(Rc::new("A".into())));
        assert_eq!(parse_regex("a").unwrap(), Symbol(Rc::new("a".into())));
        assert_eq!(
            parse_regex("CamelCase").unwrap(),
            Symbol(Rc::new("CamelCase".into()))
        );
        assert_eq!(parse_regex("0").unwrap(), EmptySet);
        assert_eq!(parse_regex("e").unwrap(), EmptyString);
    }
    #[test]
    fn test_operators() {
        assert_eq!(
            parse_regex("A*").unwrap(),
            Repeat(Rc::new(Symbol(Rc::new("A".into()))))
        );
        assert_eq!(
            parse_regex("A+").unwrap(),
            Concat(
                Rc::new(Symbol(Rc::new("A".into()))),
                Rc::new(Repeat(Rc::new(Symbol(Rc::new("A".into())))))
            )
        );
        assert_eq!(
            parse_regex("A?").unwrap(),
            Or(Rc::new(EmptyString), Rc::new(Symbol(Rc::new("A".into()))))
        );
        assert_eq!(
            parse_regex("A B").unwrap(),
            Concat(
                Rc::new(Symbol(Rc::new("A".into()))),
                Rc::new(Symbol(Rc::new("B".into())))
            )
        );
        assert_eq!(
            parse_regex("A | B").unwrap(),
            Or(
                Rc::new(Symbol(Rc::new("A".into()))),
                Rc::new(Symbol(Rc::new("B".into())))
            )
        );
        assert_eq!(
            parse_regex("A & B").unwrap(),
            And(
                Rc::new(Symbol(Rc::new("A".into()))),
                Rc::new(Symbol(Rc::new("B".into())))
            )
        );
    }
    #[test]
    fn test_disambiguation() {
        // or left-associative
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

        // closure < concat
        assert_eq!(
            parse_regex("A B*").unwrap(),
            Concat(
                Rc::new(Symbol(Rc::new("A".into()))),
                Rc::new(Repeat(Rc::new(Symbol(Rc::new("B".into()))))),
            )
        );

        // nested post-fix operators
        assert_eq!(
            parse_regex("A*?+").unwrap(),
            parse_regex("((A*)?)+").unwrap()
        );

        // not & closure < or
        assert_eq!(
            parse_regex("~A | B*").unwrap(),
            Or(
                Rc::new(Complement(Rc::new(Symbol(Rc::new("A".into()))))),
                Rc::new(Repeat(Rc::new(Symbol(Rc::new("B".into())))))
            )
        );

        // and < or
        assert_eq!(
            parse_regex("~A | B* & C?").unwrap(),
            parse_regex("(~A) | ((B*) & C?)").unwrap()
        );
    }
}
