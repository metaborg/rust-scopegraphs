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
            Self::Regex(regex) => write!(f, "{:?}", *regex),
            Self::End => write!(f, "'$'"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum ParserState {
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
    State(ParserState),
    Symbol(RegexSymbol),
    Regex(Rc<Regex>),
}

impl Debug for StackSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::State(state) => state.fmt(f),
            Self::Regex(regex) => write!(f, "({:?})", *regex),
            Self::Symbol(symbol) => symbol.fmt(f),
        }
    }
}

struct RegexParser<'a> {
    input: ParseStream<'a>,
    state: ParserState,
    next: Option<RegexSymbol>,
    symbol_stack: Vec<StackSymbol>,
}

impl<'a> RegexParser<'a> {
    fn new(input: ParseStream<'a>) -> Self {
        Self {
            input,
            state: ParserState::State0,
            next: None,
            symbol_stack: vec![StackSymbol::State(ParserState::State0)],
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

    fn shift(&mut self, new_state: ParserState) -> syn::Result<bool> {
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
            RegexSymbol::Regex(re) => self.goto(re.clone()),
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
                ParserState::State0 => ParserState::State1,
                ParserState::State1 => ParserState::State9,
                ParserState::State5 => ParserState::State12,
                ParserState::State9 => ParserState::State9,
                ParserState::State10 => ParserState::State13,
                ParserState::State11 => ParserState::State14,
                ParserState::State12 => ParserState::State9,
                ParserState::State13 => ParserState::State9,
                ParserState::State14 => ParserState::State9,
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
            ParserState::State0
            | ParserState::State5
            | ParserState::State10
            | ParserState::State11 => match self.next()? {
                RegexSymbol::Zero => self.shift(ParserState::State2),
                RegexSymbol::Epsilon => self.shift(ParserState::State3),
                RegexSymbol::Regex(_) => self.shift(ParserState::State4),
                RegexSymbol::Neg => self.shift(ParserState::State5),
                _ => self.error(
                    "expected '0', 'e', '~', label or parenthesized regular expression here",
                ),
            },
            ParserState::State1 => match self.next()? {
                RegexSymbol::Zero => self.shift(ParserState::State2),
                RegexSymbol::Epsilon => self.shift(ParserState::State3),
                RegexSymbol::Regex(_) => self.shift(ParserState::State4),
                RegexSymbol::Neg => self.shift(ParserState::State5),
                RegexSymbol::Repeat => self.shift(ParserState::State6),
                RegexSymbol::Plus => self.shift(ParserState::State7),
                RegexSymbol::Optional => self.shift(ParserState::State8),
                RegexSymbol::Or => self.shift(ParserState::State10),
                RegexSymbol::And => self.shift(ParserState::State11),
                RegexSymbol::End => Self::accept(),
            },
            ParserState::State2 => self.reduce_zero(),
            ParserState::State3 => self.reduce_epsilon(),
            ParserState::State4 => self.reduce_symbol(),
            ParserState::State6 => self.reduce_repeat(),
            ParserState::State7 => self.reduce_plus(),
            ParserState::State8 => self.reduce_optional(),
            ParserState::State9 => {
                match self.next()? {
                    // concat is right-associative, so shift in case of a new literal/pre-fix operator
                    RegexSymbol::Zero => self.shift(ParserState::State2),
                    RegexSymbol::Epsilon => self.shift(ParserState::State3),
                    RegexSymbol::Regex(_) => self.shift(ParserState::State4),
                    RegexSymbol::Neg => self.shift(ParserState::State5),
                    // post-fix operators have priority over concat, so shift here
                    RegexSymbol::Repeat => self.shift(ParserState::State6),
                    RegexSymbol::Plus => self.shift(ParserState::State7),
                    RegexSymbol::Optional => self.shift(ParserState::State8),
                    // concat has priority over '|' and  '&', so reduce here
                    RegexSymbol::Or => self.reduce_concat(),
                    RegexSymbol::And => self.reduce_concat(),
                    RegexSymbol::End => self.reduce_concat(),
                }
            }
            ParserState::State12 => {
                match self.next()? {
                    // neg has top priority, but is ambiguous with posf-fix operators.
                    RegexSymbol::Zero => self.reduce_neg(),
                    RegexSymbol::Epsilon => self.reduce_neg(),
                    RegexSymbol::Regex(_) => self.reduce_neg(),
                    RegexSymbol::Neg => self.reduce_neg(),
                    RegexSymbol::Repeat => self.error(
                        "ambiguous regex: simultaneous use of '~' prefix and '*' postfix operator",
                    ),
                    RegexSymbol::Plus => self.error(
                        "ambiguous regex: simultaneous use of '~' prefix and '+' postfix operator",
                    ),
                    RegexSymbol::Optional => self.error(
                        "ambiguous regex: simultaneous use of '~' prefix and '?' postfix operator",
                    ),
                    RegexSymbol::Or => self.reduce_neg(),
                    RegexSymbol::And => self.reduce_neg(),
                    RegexSymbol::End => self.reduce_neg(),
                }
            }
            ParserState::State13 => {
                match self.next()? {
                    // or has lowest priority, so shift in any case
                    RegexSymbol::Zero => self.shift(ParserState::State2),
                    RegexSymbol::Epsilon => self.shift(ParserState::State3),
                    RegexSymbol::Regex(_) => self.shift(ParserState::State4),
                    RegexSymbol::Neg => self.shift(ParserState::State5),
                    RegexSymbol::Repeat => self.shift(ParserState::State6),
                    RegexSymbol::Plus => self.shift(ParserState::State7),
                    RegexSymbol::Optional => self.shift(ParserState::State8),
                    // or is left-associative, so reduce eagerly
                    RegexSymbol::Or => self.reduce_or(),
                    RegexSymbol::And => self.shift(ParserState::State11),
                    RegexSymbol::End => self.reduce_or(),
                }
            }
            ParserState::State14 => {
                match self.next()? {
                    // '&' has priority over '|' only, so shift in any other case
                    RegexSymbol::Zero => self.shift(ParserState::State2),
                    RegexSymbol::Epsilon => self.shift(ParserState::State3),
                    RegexSymbol::Regex(_) => self.shift(ParserState::State4),
                    RegexSymbol::Neg => self.shift(ParserState::State5),
                    RegexSymbol::Repeat => self.shift(ParserState::State6),
                    RegexSymbol::Plus => self.shift(ParserState::State7),
                    RegexSymbol::Optional => self.shift(ParserState::State8),
                    // has priority over '|'
                    RegexSymbol::Or => self.reduce_and(),
                    // and is left-recursive, so reduce eagerly
                    RegexSymbol::And => self.reduce_and(),
                    RegexSymbol::End => self.reduce_and(),
                }
            }
        }
    }

    fn finalize(mut self) -> syn::Result<Regex> {
        let regex = self.top_regex()?;
        if let Some(StackSymbol::State(ParserState::State0)) = self.symbol_stack.pop() {
            if self.symbol_stack.is_empty() {
                Ok(regex.deref().clone())
            } else {
                self.error("error: residual input after parsing finished.")
            }
        } else {
            self.error("internal parsing error: expected state 0 on bottom of stack.")
        }
    }

    pub fn parse_regex(input: ParseStream<'a>) -> syn::Result<Regex> {
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
