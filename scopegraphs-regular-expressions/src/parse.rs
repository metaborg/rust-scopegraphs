//! This module contains a bottom-up (LR(1)) parser for our regular expression data type.
//!
//!
//! ### Grammar
//!
//! The parser is implemented based on a LR parse table generated from [an online generator](https://jsmachines.sourceforge.net/machines/lr1.html), using the following grammar.
//!
//! ```bnf
//!  0: S -> E
//!  1: E -> 0
//!  2: E -> e
//!  3: E -> l
//!  4: E -> ~ E
//!  5: E -> E *
//!  6: E -> E +
//!  7: E -> E ?
//!  8: E -> E E
//!  9: E -> E | E
//! 10: E -> E & E
//! ```
//!
//! Here, `l` should be interpreted as label literals and parenthesized expressions.
//! We do not spell out the parenthesized expressions because the Rust lexer can automatically handle those for us.
//!
//!
//! ### Disambiguation
//!
//! To disambiguate this grammar, we used the same operator priorities as [Statix](https://github.com/metaborg/nabl/blob/802559782da2216b66d290f90179c2ac8f21ba3f/statix.lang/syntax/statix/lang/Core.sdf3#L218-L221):
//!
//! | Operator            | Precedence Level | Associativity |
//! | :-----------------: | :--------------- | ------------- |
//! | `~`                 | 4                |               |
//! | `*`                 | 4                |               |
//! | `+`                 | 4                |               |
//! | `?`                 | 4                |               |
//! | (concat)            | 3                | (right)       |
//! | `&`                 | 2                | (left)        |
//! | <code>\|</code>     | 1                | (left)        |
//!
//!
//! ### Parse Table
//!
//! The LR parse table generated from this grammar is as follows:
//!
//! | State | `0`       | `e`        | `l`        | `~`        | `*`        | `+`        | `?`        | <code>\|</code> | `&`        | `$`        |   | `S` | `E`   |
//! | ----- | --------- | ---------- | ---------- | ---------- | ---------- | ---------- | ---------- | --------------- | ---------- | ---------- | - | --- | ----- |
//! | 0     | `s2`      | `s3`       | `s4`       | `s5`       |            |            |            |                 |            |            |   |     | `1`   |
//! | 1     | `s2`      | `s3`       | `s4`       | `s5`       | `s6`       | `s7`       | `s8`       | `s10`           | `s11`      | `ACC`      |   |     | `9`   |
//! | 2     | `r1`      | `r1`       | `r1`       | `r1`       | `r1`       | `r1`       | `r1`       | `r1`            | `r1`       | `r1`       |   |     |       |
//! | 3     | `r2`      | `r2`       | `r2`       | `r2`       | `r2`       | `r2`       | `r2`       | `r2`            | `r2`       | `r2`       |   |     |       |
//! | 4     | `r3`      | `r3`       | `r3`       | `r3`       | `r3`       | `r3`       | `r3`       | `r3`            | `r3`       | `r3`       |   |     |       |
//! | 5     | `s2`      | `s3`       | `s4`       | `s5`       |            |            |            |                 |            |            |   |     | `12`  |
//! | 6     | `r5`      | `r5`       | `r5`       | `r5`       | `r5`       | `r5`       | `r5`       | `r5`            | `r5`       | `r5`       |   |     |       |
//! | 7     | `r6`      | `r6`       | `r6`       | `r6`       | `r6`       | `r6`       | `r6`       | `r6`            | `r6`       | `r6`       |   |     |       |
//! | 8     | `r7`      | `r7`       | `r7`       | `r7`       | `r7`       | `r7`       | `r7`       | `r7`            | `r7`       | `r7`       |   |     |       |
//! | 9     | `s2`[^9l] | `s3`[^9l]  | `s4`[^9l]  | `s5`[^9n]  | `s6`[^9n]  | `s7`[^9n]  | `s8`[^9n]  | `r8`[^9o]       | `r8`[^9a]  | `r8`       |   |     | `9`   |
//! | 10    | `s2`      | `s3`       | `s4`       | `s5`       |            |            |            |                 |            |            |   |     | `13`  |
//! | 11    | `s2`      | `s3`       | `s4`       | `s5`       |            |            |            |                 |            |            |   |     | `14`  |
//! | 12    | `r4`[^120]| `r4`[^12e] | `r4`[^12l] | `r4`[^12n] | `AMB`[^2b] | `AMB`[^2b] | `AMB`[^2b] | `r4`[^12o]      | `r4`[^12a] | `r4`       |   |     | `9`   |
//! | 13    | `s2`[^3l] | `s3`[^3l]  | `s4`[^3l]  | `s5`[^3l]  | `s6`[^3l]  | `s7`[^3l]  | `s8`[^3l]  | `r9`[^3o]       | `s11`[^3l] | `r9`       |   |     | `9`   |
//! | 14    | `s2`[^4l] | `s3`[^4l]  | `s4`[^4l]  | `s5`[^4l]  | `s6`[^4l]  | `s7`[^4l]  | `s8`[^4l]  | `r10`[^4o]      | `r10`[^4a] | `r10`      |   |     | `9`   |
//!
//! The first segment being the ACTION-table, the last two columns being the GOTO-table.
//! In the action table, actions as `sX` means: shift the symbol onto the stack, transition to state `X`.
//! Actions like `rY` mean: reduce the top of the stack using production `Y` (from original grammar).
//! The `ACC` action means accepting the current input.
//! The `AMB` action is emitting an error that the current expression is ambiguous.
//! Empty boxes are parse errors: the tokens are not expected at that position.
//! The resolution of ambiguities (positions in the table where both a shift and a reduce action are possible (shift/reduce conflicts)) are annotated and their resolution motivated in the footnotes.
//!
//!
//! ### Implementation
//!
//! This module contains a lexer ([`RegexLexer`]), which is a wrapper around a [`ParseStream`] that emits [`RegexSymbol`]s (the tokens of our language).
//! Using [`parenthesized`], we recursively parse regular expressions inside brackets.
//!
//! The parser implementation ([`RegexParser`]) corresponds to the parse table in the following way:
//! - The [`RegexParser::shift`] function implements shifting and transitioning to some state.
//! - The [`RegexParser::goto`] function implements the GOTO-table.
//! - The [`RegexParser::accept`] function accepts the input, leaving the resulting regular expression on the stack.
//! - The [`RegexParser::step`] function performs a parsing step per invocation (i.e., it implements the ACTION-table).
//!
//! The reduction rules each have their own function as well:
//! - `r1`: [`RegexParser::reduce_zero`]
//! - `r2`: [`RegexParser::reduce_epsilon`]
//! - `r3`: [`RegexParser::reduce_symbol`] (also parses parenthesized expressions)
//! - `r4`: [`RegexParser::reduce_neg`]
//! - `r5`: [`RegexParser::reduce_repeat`]
//! - `r6`: [`RegexParser::reduce_plus`]
//! - `r7`: [`RegexParser::reduce_optional`]
//! - `r8`: [`RegexParser::reduce_concat`]
//! - `r9`: [`RegexParser::reduce_or`]
//! - `r10`: [`RegexParser::reduce_and`]
//!
//! The parser does not have error recovery.
//!
//!
//! [^9l]: shift/reduce conflict with `r8`: resolved this way because concatenation is right-associative, so we need to shift more before we can reduce.
//!
//! [^9n]: shift/reduce conflict with `r8`: resolved this way because `~`/`*`/`+`/`?` have priority over concatenation.
//!
//! [^9o]: shift/reduce conflict with `s8`: resolved as `r8` because concatenation has priority over `|`.
//!
//! [^9a]: shift/reduce conflict with `s10`: resolved as `r8` because concatenation has priority over `&`.
//!
//!
//!
//! [^120]: shift/reduce conflict with `s2`: resolved as `r4` because negation has priority over concatenation.
//!
//! [^12e]: shift/reduce conflict with `s3`: resolved as `r4` because negation has priority over concatenation.
//!
//! [^12l]: shift/reduce conflict with `s4`: resolved as `r4` because negation has priority over concatenation.
//!
//! [^12n]: shift/reduce conflict with `s2`: resolved as `r4` because negation has priority over concatenation.
//!
//! [^2b]:  shift/reduce conflict with `s2`: resolved as an _ambiguity error_ because there is no priority between `~` (prefix operator) and `*`/`+`/`?` (postfix operators).
//!
//! [^12o]: shift/reduce conflict with `s2`: resolved as `r4` because negation has priority over `|`.
//!
//! [^12a]: shift/reduce conflict with `s2`: resolved as `r4` because negation has priority over `&`.
//!
//!
//!
//! [^3l]: shift/reduce conflict with `r9`: resolved as a shift because `|` has lowest priority.
//!
//! [^3o]: shift/reduce conflict with `s10`: resolved as a `r9` because `|` is left-associative.
//!
//!
//!
//! [^4l]: shift/reduce conflict with `r10`: resolved as a shift because `&` has lower priority than all operators, except for `|`.
//!
//! [^4o]: shift/reduce conflict with `s10`: resolved as a `r10` because `&` is left-associative.
//!
//! [^4a]: shift/reduce conflict with `s11`: resolved as a `r10` because `&` has priority over `|`.

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

/// In regular LR parsing literature, the parse stack is an alternating sequence of states and symbols.
/// It starts with the initial state [`ParserState::State0`], and always (except between a reduction and a goto) has a state on top.
/// We differ from this representation in two ways:
/// 1. We do not explicitly push the initial state to the stack, but implicitly assume it in [`RegexParser::goto`].
/// 2. We combine the reduce and goto actions.
///    The reduce actions do not push the result to the stack, but pass it to [`RegexParser::goto`] directly.
///    [`RegexParser::goto`] computes the next state based on the top of the stack and the result that was passed in, and pushes the result and the new state to the stack.
/// This allows us to combine the stack elements as 2-tuples, getting rid of a lot of runtime checks & pattern matching.
#[derive(Clone)]
struct StackSymbol {
    state: ParserState,
    symbol: RegexSymbol,
}

impl Debug for StackSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:?}, {:?}>", self.state, self.symbol)
    }
}

struct RegexLexer<'a> {
    input: ParseStream<'a>,
    top: RegexSymbol,
}

impl<'a> RegexLexer<'a> {
    /// Returns the next [`RegexSymbol`] from the `input`.
    fn scan(input: ParseStream<'a>) -> syn::Result<RegexSymbol> {
        if input.is_empty() {
            return Ok(RegexSymbol::End);
        }

        let lookahead = input.lookahead1();

        // Rust performs parenthesis matching: leverage that here.
        if lookahead.peek(syn::token::Paren) {
            let inner;
            parenthesized!(inner in input);
            return Regex::parse(&inner).map(|re| RegexSymbol::Regex(Rc::new(re)));
        }

        // Scan '0' symbol
        if let Ok(val) = input.parse::<syn::LitInt>() {
            if let Ok(0) = val.base10_parse() {
                return Ok(RegexSymbol::Zero);
            }
        }

        // Scan 'e' and names
        if let Ok(name) = input.parse::<Path>() {
            if name.is_ident("e") {
                return Ok(RegexSymbol::Epsilon);
            } else {
                let regex = Regex::Symbol(Symbol { name }.into());
                return Ok(RegexSymbol::Regex(Rc::new(regex)));
            }
        }

        // Scan '~' symbol
        if input.parse::<Token![~]>().is_ok() {
            return Ok(RegexSymbol::Neg);
        }

        // Scan '*' symbol
        if input.parse::<Token![*]>().is_ok() {
            return Ok(RegexSymbol::Repeat);
        }

        // Scan '+' symbol
        if input.parse::<Token![+]>().is_ok() {
            return Ok(RegexSymbol::Plus);
        }

        // Scan '?' symbol
        if input.parse::<Token![?]>().is_ok() {
            return Ok(RegexSymbol::Optional);
        }

        // Scan '|' symbol
        if input.parse::<Token![|]>().is_ok() {
            return Ok(RegexSymbol::Or);
        }

        // Scan '&' symbol
        if input.parse::<Token![&]>().is_ok() {
            return Ok(RegexSymbol::And);
        }

        Err(lookahead.error())
    }

    /// Peeks the first symbol in the stream.
    fn peek(&self) -> &RegexSymbol {
        &self.top
    }

    /// Advances lexer to the next token.
    fn next(&mut self) -> syn::Result<RegexSymbol> {
        let sym = self.top.clone();
        self.top = Self::scan(self.input)?;
        Ok(sym)
    }

    /// Returns the current position in the stream (mainly for emitting errors).
    fn span(&self) -> proc_macro2::Span {
        self.input.span()
    }
}

struct RegexParser<'a> {
    lexer: RegexLexer<'a>,
    state: ParserState,
    symbol_stack: Vec<StackSymbol>,
}

impl<'a> RegexParser<'a> {
    /// Initializer a new parser for the underlying token stream.
    fn init(input: ParseStream<'a>) -> syn::Result<Self> {
        let lexer = RegexLexer {
            input,
            top: RegexLexer::scan(input)?,
        };
        Ok(Self {
            lexer,
            state: ParserState::State0,
            symbol_stack: vec![],
        })
    }

    /// Shift action (standars in LR parsers).
    ///
    /// Pushes current token to the stack, and transitions to `new_state`.
    fn shift(&mut self, new_state: ParserState) -> syn::Result<bool> {
        let next_symbol = self.lexer.next()?;
        self.symbol_stack.push(StackSymbol {
            state: new_state,
            symbol: next_symbol,
        });
        self.state = new_state;
        Ok(false) // not (yet) in accepting state
    }

    /// Get last [symbol](RegexSymbol) shifted to the stack.
    fn top_stack_symbol(&mut self) -> syn::Result<RegexSymbol> {
        if let Some(symbol) = self.symbol_stack.pop() {
            Ok(symbol.symbol)
        } else {
            self.internal_error("expected non-empty stack")
        }
    }

    /// Get last symbol shifted to the stack, and assert it is a [`StackSymbol::Regex`] variant.
    fn top_regex(&mut self) -> syn::Result<Rc<Regex>> {
        if let RegexSymbol::Regex(regex) = self.top_stack_symbol()? {
            Ok(regex)
        } else {
            self.internal_error("expected regex on top of stack")
        }
    }

    /// Get last symbol shifted to the stack, and assert it is a particular `expected` [`RegexSymbol`].
    fn expect_symbol(&mut self, expected: RegexSymbol) -> syn::Result<()> {
        if expected == self.top_stack_symbol()? {
            Ok(())
        } else {
            self.internal_error("expected other symbol on top of stack")
        }
    }

    // reduce atoms

    fn reduce_atom(&mut self, expected: RegexSymbol, result: Regex) -> syn::Result<bool> {
        self.expect_symbol(expected)?;
        self.goto(Rc::new(result))
    }

    /// Reduce `E -> 0` preduction
    fn reduce_zero(&mut self) -> syn::Result<bool> {
        self.reduce_atom(RegexSymbol::Zero, Regex::EmptySet)
    }

    /// Reduce `E -> e` preduction.
    fn reduce_epsilon(&mut self) -> syn::Result<bool> {
        self.reduce_atom(RegexSymbol::Epsilon, Regex::EmptyString)
    }

    /// Reduce `E -> l` and `E -> ( E )` productions (NO-OP: symbol is shared).
    fn reduce_symbol(&mut self) -> syn::Result<bool> {
        let re = self.top_regex()?;
        self.goto(re)
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

    /// Reduce `E -> ~ E` production.
    fn reduce_neg(&mut self) -> syn::Result<bool> {
        self.reduce_unary_prefix(RegexSymbol::Neg, Regex::Complement)
    }

    fn reduce_unary_postfix(
        &mut self,
        expected: RegexSymbol,
        build: impl Fn(Rc<Regex>) -> Regex,
    ) -> syn::Result<bool> {
        self.expect_symbol(expected)?;
        let re = self.top_regex()?;
        self.goto(Rc::new(build(re)))
    }

    /// Reduce `E -> E *` production.
    fn reduce_repeat(&mut self) -> syn::Result<bool> {
        self.reduce_unary_postfix(RegexSymbol::Repeat, Regex::Repeat)
    }

    /// Reduce `E -> E +` production.
    ///
    /// Immediately desugars `E1 +` to `E1 E1*`.
    fn reduce_plus(&mut self) -> syn::Result<bool> {
        self.reduce_unary_postfix(RegexSymbol::Plus, |re| {
            Regex::Concat(re.clone(), Rc::new(Regex::Repeat(re)))
        })
    }

    /// Reduce `E -> E ?` production.
    ///
    /// Immediately desugars `E1 ?` to `e | E1`.
    fn reduce_optional(&mut self) -> syn::Result<bool> {
        self.reduce_unary_postfix(RegexSymbol::Optional, |re| {
            Regex::Or(Rc::new(Regex::EmptyString), re)
        })
    }

    // reduce binary operators

    /// Reduce `E -> E E` production.
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
        self.expect_symbol(expected)?;
        let l = self.top_regex()?;
        self.goto(Rc::new(build(l, r)))
    }

    /// Reduce `E -> E | E` production.
    fn reduce_or(&mut self) -> syn::Result<bool> {
        self.reduce_binary_infix(RegexSymbol::Or, Regex::Or)
    }

    /// Reduce `E -> E & E` production.
    fn reduce_and(&mut self) -> syn::Result<bool> {
        self.reduce_binary_infix(RegexSymbol::And, Regex::And)
    }

    /// Implement GOTO-table.
    ///
    /// Called after a reduce operation.
    /// The `result` argument is the result of the reduction (which is always of sort `E`).
    ///
    /// It peeks the state `S` on top of the stack (assuming `0` in case of an empty stack).
    /// Then it finds the `GOTO[S, E]` entry, and pushes the result and the new state to the stack.
    fn goto(&mut self, result: Rc<Regex>) -> syn::Result<bool> {
        let st = self
            .symbol_stack
            .last()
            .map(|ss| ss.state)
            .unwrap_or(ParserState::State0);
        let state = match st {
            ParserState::State0 => ParserState::State1,
            ParserState::State1 => ParserState::State9,
            ParserState::State5 => ParserState::State12,
            ParserState::State9 => ParserState::State9,
            ParserState::State10 => ParserState::State13,
            ParserState::State11 => ParserState::State14,
            ParserState::State12 => ParserState::State9,
            ParserState::State13 => ParserState::State9,
            ParserState::State14 => ParserState::State9,
            _ => return self.internal_error("cannot perform 'goto' action on current state"),
        };
        self.symbol_stack.push(StackSymbol {
            state,
            symbol: RegexSymbol::Regex(result),
        });
        self.state = state;
        Ok(false)
    }

    /// Accepts the input.
    fn accept() -> syn::Result<bool> {
        Ok(true)
    }

    fn build_error(&mut self, msg: &str) -> syn::Error {
        syn::Error::new(self.lexer.span(), msg)
    }

    fn error<T>(&mut self, msg: &str) -> syn::Result<T> {
        Err(self.build_error(msg))
    }

    fn internal_error<T>(&mut self, msg: &str) -> syn::Result<T> {
        Err(self.build_error(&format!("internal parsing error: {}", msg)))
    }

    /// Implementation of the ACTION-table.
    fn step(&mut self) -> syn::Result<bool> {
        match self.state {
            ParserState::State0
            | ParserState::State5
            | ParserState::State10
            | ParserState::State11 => match self.lexer.peek() {
                RegexSymbol::Zero => self.shift(ParserState::State2),
                RegexSymbol::Epsilon => self.shift(ParserState::State3),
                RegexSymbol::Regex(_) => self.shift(ParserState::State4),
                RegexSymbol::Neg => self.shift(ParserState::State5),
                _ => self.error(
                    "expected '0', 'e', '~', label or parenthesized regular expression here",
                ),
            },
            ParserState::State1 => match self.lexer.peek() {
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
            ParserState::State9 => match self.lexer.peek() {
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
            },
            ParserState::State12 => match self.lexer.peek() {
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
            },
            ParserState::State13 => match self.lexer.peek() {
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
            },
            ParserState::State14 => match self.lexer.peek() {
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
            },
        }
    }

    /// Extracts parsing result from the stack.
    fn finalize(mut self) -> syn::Result<Regex> {
        let regex = self.top_regex()?;
        if self.symbol_stack.is_empty() {
            Ok(regex.deref().clone())
        } else {
            self.internal_error("residual input after parsing finished.")
        }
    }

    /// Entry point: parses the `input` to a [`Regex`].
    pub fn parse_regex(input: ParseStream) -> syn::Result<Regex> {
        let mut parser = RegexParser::init(input)?;
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
