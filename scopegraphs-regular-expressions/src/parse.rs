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
//! The LR parse table generated from this grammar is as follows (closure sets on the documentation of [`ParseState`]):
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
//! For more explanation on constructing the parse table, see [these slides](https://www.eecis.udel.edu/~cavazos/cisc672-fall08/lectures/Lecture-10.pdf).
//!
//!
//! ### Implementation
//!
//! This module contains a lexer ([`Lexer`]), which is a wrapper around a [`ParseStream`] that emits [`Token`]s (the tokens of our language).
//! Using [`parenthesized`], we recursively parse regular expressions inside parentheses.
//!
//! The parser implementation ([`Parser`]) corresponds to the parse table in the following way:
//! - The [`Parser::shift`] function implements shifting and transitioning to some state.
//! - The [`Parser::goto`] function implements the GOTO-table.
//! - The [`Parser::accept`] function accepts the input, leaving the resulting regular expression on the stack.
//! - The [`Parser::step`] function performs a parsing step per invocation (i.e., it implements the ACTION-table).
//!
//! The reduction rules each have their own function as well:
//! - `r1`: [`Parser::reduce_zero`]
//! - `r2`: [`Parser::reduce_epsilon`]
//! - `r3`: [`Parser::reduce_symbol`] (also parses parenthesized expressions)
//! - `r4`: [`Parser::reduce_neg`]
//! - `r5`: [`Parser::reduce_repeat`]
//! - `r6`: [`Parser::reduce_plus`]
//! - `r7`: [`Parser::reduce_optional`]
//! - `r8`: [`Parser::reduce_concat`]
//! - `r9`: [`Parser::reduce_or`]
//! - `r10`: [`Parser::reduce_and`]
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
enum Token {
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

impl Debug for Token {
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

struct Lexer<'a> {
    input: ParseStream<'a>,
    top: Token,
}

impl<'a> Lexer<'a> {
    /// Returns the next [`Token`] from the `input`.
    fn scan(input: ParseStream<'a>) -> syn::Result<Token> {
        if input.is_empty() {
            return Ok(Token::End);
        }

        let lookahead = input.lookahead1();

        // Rust performs parenthesis matching: leverage that here.
        if lookahead.peek(syn::token::Paren) {
            let inner;
            parenthesized!(inner in input);
            return Regex::parse(&inner).map(|re| Token::Regex(Rc::new(re)));
        }

        // Scan '0' token
        if let Ok(val) = input.parse::<syn::LitInt>() {
            if let Ok(0) = val.base10_parse() {
                return Ok(Token::Zero);
            }
        }

        // Scan 'e' and names
        if let Ok(name) = input.parse::<Path>() {
            if name.is_ident("e") {
                return Ok(Token::Epsilon);
            } else {
                let regex = Regex::Symbol(Symbol { name }.into());
                return Ok(Token::Regex(Rc::new(regex)));
            }
        }

        // Scan '~' token
        if input.parse::<Token![~]>().is_ok() {
            return Ok(Token::Neg);
        }

        // Scan '*' token
        if input.parse::<Token![*]>().is_ok() {
            return Ok(Token::Repeat);
        }

        // Scan '+' token
        if input.parse::<Token![+]>().is_ok() {
            return Ok(Token::Plus);
        }

        // Scan '?' token
        if input.parse::<Token![?]>().is_ok() {
            return Ok(Token::Optional);
        }

        // Scan '|' token
        if input.parse::<Token![|]>().is_ok() {
            return Ok(Token::Or);
        }

        // Scan '&' token
        if input.parse::<Token![&]>().is_ok() {
            return Ok(Token::And);
        }

        Err(lookahead.error())
    }

    /// Peeks the first token in the stream.
    fn peek(&self) -> &Token {
        &self.top
    }

    /// Advances lexer to the next token.
    fn next(&mut self) -> syn::Result<Token> {
        let token = self.top.clone();
        self.top = Self::scan(self.input)?;
        Ok(token)
    }

    /// Returns the current position in the stream (mainly for emitting errors).
    fn span(&self) -> proc_macro2::Span {
        self.input.span()
    }
}

/// Enumeration of the states of the parser.
///
/// The state names are chosen to represent the items currently on top of the stack.
/// For example `FooBar` means that a `Bar` symbol is on top of the stack, and a `Foo` right below.
/// Ordinal numbers correspond to the state numbers in top-level docs.
///
/// The documentation of each item contains its closure set.
/// If not documented, the lookahead is `$/*/+/?/0/e/l/~/|/&` (i.e., all tokens).
#[derive(Clone, Copy, Debug)]
enum ParseState {
    /// Initial state of the parser.
    ///
    /// Closure set:
    /// - [S -> .E, $]
    /// - [E -> .0]
    /// - [E -> .e]
    /// - [E -> .l]
    /// - [E -> .~ E]
    /// - [E -> .E *]
    /// - [E -> .E +]
    /// - [E -> .E ?]
    /// - [E -> .E E]
    /// - [E -> .E | E]
    /// - [E -> .E & E]
    Initial = 0,
    /// State after reducing a top-level regular expression.
    /// Might accept when the end of the stream is reached, or continue parsing a infix/post-fix operation.
    ///
    /// Closure set:    
    /// - [S -> E., $]
    /// - [E -> E.*]
    /// - [E -> E.+]
    /// - [E -> E.?]
    /// - [E -> E.E]
    /// - [E -> E.| E]
    /// - [E -> E.& E]
    /// - [E -> .0]
    /// - [E -> .e]
    /// - [E -> .l]
    /// - [E -> .~ E]
    /// - [E -> .E *]
    /// - [E -> .E +]
    /// - [E -> .E ?]
    /// - [E -> .E E]
    /// - [E -> .E | E]
    /// - [E -> .E & E]
    Regex = 1,
    /// State after shifting a `0`: will always reduce using `E -> 0`.
    ///
    /// Closure set:
    /// - [E -> 0.]
    Zero = 2,
    /// State after shifting a `e`: will always reduce using `E -> e`.
    ///
    /// Closure set:
    /// - [E -> e.]
    Epsilon = 3,
    /// State after shifting a `l` or parenthesized expression: will always reduce using `E -> l` or `E -> ( E )`.
    ///
    /// Closure set:
    /// - [E -> l.]
    /// - [E -> ( E ).] (manually added)
    Symbol = 4,
    /// State after shifting a `~`.
    ///
    /// Closure set:
    /// - [E -> ~.E]
    /// - [E -> .0]
    /// - [E -> .e]
    /// - [E -> .l]
    /// - [E -> .~ E]
    /// - [E -> .E *]
    /// - [E -> .E +]
    /// - [E -> .E ?]
    /// - [E -> .E E]
    /// - [E -> .E | E]
    /// - [E -> .E & E]
    Neg = 5,
    /// State after shifting a `*`.
    /// Will always reduce using `E -> E *`.
    ///
    /// Closure set:
    /// - [E -> E *.]
    RegexRepeat = 6,
    /// State after shifting a `+`.
    /// Will always reduce using `E -> E +`.
    ///
    /// Closure set:
    /// - [E -> E +.]
    RegexPlus = 7,
    /// State after shifting a `?`.
    /// Will always reduce using `E -> E ?`.
    ///
    /// Closure set:
    /// - [E -> E ?.]
    RegexOptional = 8,
    /// State two fully reduced regular expressions are at the top of the stack.
    /// Can further reduce concat operations, or shift inputs with higher priority.
    ///
    /// Closure set:
    /// - [E -> E E.]
    /// - [E -> E.*]
    /// - [E -> E.+]
    /// - [E -> E.?]
    /// - [E -> E.E]
    /// - [E -> E.| E]
    /// - [E -> E.& E]
    /// - [E -> .0]
    /// - [E -> .e]
    /// - [E -> .l]
    /// - [E -> .~ E]
    /// - [E -> .E *]
    /// - [E -> .E +]
    /// - [E -> .E ?]
    /// - [E -> .E E]
    /// - [E -> .E | E]
    /// - [E -> .E & E]
    RegexRegex = 9,
    /// State after shifting a `|`.
    /// Can reduce using `E -> E | E`, or shift inputs with higher priority.
    ///
    /// Closure set:
    /// - [E -> E |.E]
    /// - [E -> .0]
    /// - [E -> .e]
    /// - [E -> .l]
    /// - [E -> .~ E]
    /// - [E -> .E *]
    /// - [E -> .E +]
    /// - [E -> .E ?]
    /// - [E -> .E E]
    /// - [E -> .E | E]
    /// - [E -> .E & E]
    RegexOr = 10,
    /// State after shifting a `&`.
    /// Can reduce using `E -> E & E`, or shift inputs with higher priority.
    ///
    /// Closure set:
    /// - [E -> E &.E]
    /// - [E -> .0]
    /// - [E -> .e]
    /// - [E -> .l]
    /// - [E -> .~ E]
    /// - [E -> .E *]
    /// - [E -> .E +]
    /// - [E -> .E ?]
    /// - [E -> .E E]
    /// - [E -> .E | E]
    /// - [E -> .E & E]
    RegexAnd = 11,
    /// State when a negation operator and a RE are at the top of the stack.
    /// Can reduce using the `E -> ~ E` production, or give an ambiguity error with other unary operators.
    ///
    /// Closure set:
    /// - [E -> ~ E.]
    /// - [E -> E.*]
    /// - [E -> E.+]
    /// - [E -> E.?]
    /// - [E -> E.E]
    /// - [E -> E.| E]
    /// - [E -> E.& E]
    /// - [E -> .0]
    /// - [E -> .e]
    /// - [E -> .l]
    /// - [E -> .~ E]
    /// - [E -> .E *]
    /// - [E -> .E +]
    /// - [E -> .E ?]
    /// - [E -> .E E]
    /// - [E -> .E | E]
    /// - [E -> .E & E]
    NegRegex = 12,
    /// State when an RE, an or-operator and another RE are at the top of the stack.
    /// Can reduce using the `E -> E | E` production, or shift higher priority operators.
    ///
    /// Closure set:
    /// - [E -> E | E.]
    /// - [E -> E.*]
    /// - [E -> E.+]
    /// - [E -> E.?]
    /// - [E -> E.E]
    /// - [E -> E.| E]
    /// - [E -> E.& E]
    /// - [E -> .0]
    /// - [E -> .e]
    /// - [E -> .l]
    /// - [E -> .~ E]
    /// - [E -> .E *]
    /// - [E -> .E +]
    /// - [E -> .E ?]
    /// - [E -> .E E]
    /// - [E -> .E | E]
    /// - [E -> .E & E]
    RegexOrRegex = 13,
    /// State when an RE, an and-operator and another RE are at the top of the stack.
    /// Can reduce using the `E -> E & E` production, or shift higher priority operators.
    ///
    /// Closure set:
    /// - [E -> E & E.]
    /// - [E -> E.*]
    /// - [E -> E.+]
    /// - [E -> E.?]
    /// - [E -> E.E]
    /// - [E -> E.| E]
    /// - [E -> E.& E]
    /// - [E -> .0]
    /// - [E -> .e]
    /// - [E -> .l]
    /// - [E -> .~ E]
    /// - [E -> .E *]
    /// - [E -> .E +]
    /// - [E -> .E ?]
    /// - [E -> .E E]
    /// - [E -> .E | E]
    /// - [E -> .E & E]
    RegexAndRegex = 14,
}

/// Segment of the parse stack.
///
/// In regular LR parsing literature, the parse stack is an alternating sequence of states and symbols.
/// It starts with the initial state [`ParseState::Initial`], and always (except between a reduction and a goto) has a state on top.
/// We differ from this representation in two ways:
/// 1. We do not explicitly push the initial state to the stack, but implicitly assume it in [`Parser::state`].
/// 2. We combine the reduce and goto actions.
///    The reduce actions do not push the result to the stack, but pass it to [`Parser::goto`] directly.
///    [`Parser::goto`] computes the next state based on the top of the stack and the result that was passed in, and pushes the result and the new state to the stack.
///
/// This allows us to combine the stack elements as 2-tuples, getting rid of a lot of runtime checks & pattern matching.
#[derive(Clone)]
struct StackSegment {
    state: ParseState,
    token: Token,
}

impl Debug for StackSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:?}, {:?}>", self.state, self.token)
    }
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    parse_stack: Vec<StackSegment>,
}

impl<'a> Parser<'a> {
    /// Initializer a new parser for the underlying token stream.
    fn init(input: ParseStream<'a>) -> syn::Result<Self> {
        let lexer = Lexer {
            input,
            top: Lexer::scan(input)?,
        };
        Ok(Self {
            lexer,
            parse_stack: vec![],
        })
    }

    /// Shift action (standard in LR parsers).
    ///
    /// Pushes current token to the stack, and transitions to `new_state`.
    fn shift(&mut self, new_state: ParseState) -> syn::Result<bool> {
        let next_token = self.lexer.next()?;
        self.parse_stack.push(StackSegment {
            state: new_state,
            token: next_token,
        });
        Ok(false) // not (yet) in accepting state
    }

    /// Get last [state](ParseState) shifted to the stack.
    ///
    /// Defaults to the (implicit) [initial state](ParseState::Initial) in case of an empty stack.
    fn state(&self) -> ParseState {
        self.parse_stack
            .last()
            .map(|ss| ss.state)
            .unwrap_or(ParseState::Initial) // implicitly assume State 0 on bottom of stack.
    }

    /// Pop last state and token from the stack.
    fn pop_stack(&mut self) -> syn::Result<StackSegment> {
        if let Some(segment) = self.parse_stack.pop() {
            Ok(segment)
        } else {
            self.internal_error("expected non-empty stack")
        }
    }

    /// Get last [token](Token) shifted to the stack.
    fn pop_stack_token(&mut self) -> syn::Result<Token> {
        self.pop_stack().map(|ss| ss.token)
    }

    /// Get last token shifted to the stack, and assert it is a [`Token::Regex`] variant.
    fn pop_stack_regex(&mut self) -> syn::Result<Rc<Regex>> {
        if let Token::Regex(regex) = self.pop_stack_token()? {
            Ok(regex)
        } else {
            self.internal_error("expected regex on top of stack")
        }
    }

    /// Get last token shifted to the stack, and assert it is a particular `expected` [`Token`].
    fn expect_token(&mut self, expected: Token) -> syn::Result<()> {
        if expected == self.pop_stack_token()? {
            Ok(())
        } else {
            self.internal_error("expected other token on top of stack")
        }
    }

    // reduce atoms

    fn reduce_atom(&mut self, expected: Token, result: Regex) -> syn::Result<bool> {
        self.expect_token(expected)?;
        self.goto(Rc::new(result))
    }

    /// Reduce `E -> 0` preduction
    fn reduce_zero(&mut self) -> syn::Result<bool> {
        self.reduce_atom(Token::Zero, Regex::EmptySet)
    }

    /// Reduce `E -> e` preduction.
    fn reduce_epsilon(&mut self) -> syn::Result<bool> {
        self.reduce_atom(Token::Epsilon, Regex::EmptyString)
    }

    /// Reduce `E -> l` and `E -> ( E )` productions (NO-OP: symbol is shared).
    fn reduce_symbol(&mut self) -> syn::Result<bool> {
        let re = self.pop_stack_regex()?;
        self.goto(re)
    }

    // reduce unary operators

    fn reduce_unary_prefix(
        &mut self,
        expected: Token,
        build: impl Fn(Rc<Regex>) -> Regex,
    ) -> syn::Result<bool> {
        let re = self.pop_stack_regex()?;
        self.reduce_atom(expected, build(re))
    }

    /// Reduce `E -> ~ E` production.
    fn reduce_neg(&mut self) -> syn::Result<bool> {
        self.reduce_unary_prefix(Token::Neg, Regex::Complement)
    }

    fn reduce_unary_postfix(
        &mut self,
        expected: Token,
        build: impl Fn(Rc<Regex>) -> Regex,
    ) -> syn::Result<bool> {
        self.expect_token(expected)?;
        let re = self.pop_stack_regex()?;
        self.goto(Rc::new(build(re)))
    }

    /// Reduce `E -> E *` production.
    fn reduce_repeat(&mut self) -> syn::Result<bool> {
        self.reduce_unary_postfix(Token::Repeat, Regex::Repeat)
    }

    /// Reduce `E -> E +` production.
    ///
    /// Immediately desugars `E1 +` to `E1 E1*`.
    fn reduce_plus(&mut self) -> syn::Result<bool> {
        self.reduce_unary_postfix(Token::Plus, |re| {
            Regex::Concat(re.clone(), Rc::new(Regex::Repeat(re)))
        })
    }

    /// Reduce `E -> E ?` production.
    ///
    /// Immediately desugars `E1 ?` to `e | E1`.
    fn reduce_optional(&mut self) -> syn::Result<bool> {
        self.reduce_unary_postfix(Token::Optional, |re| {
            Regex::Or(Rc::new(Regex::EmptyString), re)
        })
    }

    // reduce binary operators

    /// Reduce `E -> E E` production.
    fn reduce_concat(&mut self) -> syn::Result<bool> {
        // right-hand-side is on top of stack ...
        let r = self.pop_stack_regex()?;
        let l = self.pop_stack_regex()?;
        self.goto(Rc::new(Regex::Concat(l, r)))
    }

    fn reduce_binary_infix(
        &mut self,
        expected: Token,
        build: impl Fn(Rc<Regex>, Rc<Regex>) -> Regex,
    ) -> syn::Result<bool> {
        // right-hand-side is on top of stack ...
        let r = self.pop_stack_regex()?;
        self.expect_token(expected)?;
        let l = self.pop_stack_regex()?;
        self.goto(Rc::new(build(l, r)))
    }

    /// Reduce `E -> E | E` production.
    fn reduce_or(&mut self) -> syn::Result<bool> {
        self.reduce_binary_infix(Token::Or, Regex::Or)
    }

    /// Reduce `E -> E & E` production.
    fn reduce_and(&mut self) -> syn::Result<bool> {
        self.reduce_binary_infix(Token::And, Regex::And)
    }

    /// Implement GOTO-table.
    ///
    /// Called after a reduce operation.
    /// The `result` argument is the result of the reduction (which is always of sort `E`).
    ///
    /// It peeks the state `S` on top of the stack (assuming `0` in case of an empty stack).
    /// Then it finds the `GOTO[S, E]` entry, and pushes the result and the new state to the stack.
    fn goto(&mut self, result: Rc<Regex>) -> syn::Result<bool> {
        // compute state that has a new regex pushed to the stack
        let state = match self.state() {
            ParseState::Initial => ParseState::Regex,
            ParseState::Regex => ParseState::RegexRegex,
            ParseState::Neg => ParseState::NegRegex,
            ParseState::RegexRegex => ParseState::RegexRegex,
            ParseState::RegexOr => ParseState::RegexOrRegex,
            ParseState::RegexAnd => ParseState::RegexAndRegex,
            ParseState::NegRegex => ParseState::RegexRegex, // redundant: negations are always reduced eagerly
            ParseState::RegexOrRegex => ParseState::RegexRegex,
            ParseState::RegexAndRegex => ParseState::RegexRegex,
            _ => return self.internal_error("cannot perform 'goto' action on current state"),
        };
        self.parse_stack.push(StackSegment {
            state,
            token: Token::Regex(result),
        });
        Ok(false)
    }

    /// Accepts the input.
    fn accept() -> syn::Result<bool> {
        Ok(true)
    }

    fn build_error(&self, msg: &str) -> syn::Error {
        syn::Error::new(self.lexer.span(), msg)
    }

    fn error<T>(&self, msg: &str) -> syn::Result<T> {
        Err(self.build_error(msg))
    }

    fn internal_error<T>(&self, msg: &str) -> syn::Result<T> {
        Err(self.build_error(&format!("internal parsing error: {}", msg)))
    }

    /// Implementation of the ACTION-table.
    fn step(&mut self) -> syn::Result<bool> {
        match self.state() {
            ParseState::Initial | ParseState::Neg | ParseState::RegexOr | ParseState::RegexAnd => {
                match self.lexer.peek() {
                    Token::Zero => self.shift(ParseState::Zero),
                    Token::Epsilon => self.shift(ParseState::Epsilon),
                    Token::Regex(_) => self.shift(ParseState::Symbol),
                    Token::Neg => self.shift(ParseState::Neg),
                    _ => self.error(
                        "expected '0', 'e', '~', label or parenthesized regular expression here",
                    ),
                }
            }
            ParseState::Regex => match self.lexer.peek() {
                Token::Zero => self.shift(ParseState::Zero),
                Token::Epsilon => self.shift(ParseState::Epsilon),
                Token::Regex(_) => self.shift(ParseState::Symbol),
                Token::Neg => self.shift(ParseState::Neg),
                Token::Repeat => self.shift(ParseState::RegexRepeat),
                Token::Plus => self.shift(ParseState::RegexPlus),
                Token::Optional => self.shift(ParseState::RegexOptional),
                Token::Or => self.shift(ParseState::RegexOr),
                Token::And => self.shift(ParseState::RegexAnd),
                Token::End => Self::accept(),
            },
            ParseState::Zero => self.reduce_zero(),
            ParseState::Epsilon => self.reduce_epsilon(),
            ParseState::Symbol => self.reduce_symbol(),
            ParseState::RegexRepeat => self.reduce_repeat(),
            ParseState::RegexPlus => self.reduce_plus(),
            ParseState::RegexOptional => self.reduce_optional(),
            ParseState::RegexRegex => match self.lexer.peek() {
                // concat is right-associative, so shift in case of a new literal/pre-fix operator
                Token::Zero => self.shift(ParseState::Zero),
                Token::Epsilon => self.shift(ParseState::Epsilon),
                Token::Regex(_) => self.shift(ParseState::Symbol),
                Token::Neg => self.shift(ParseState::Neg),
                // post-fix operators have priority over concat, so shift here
                Token::Repeat => self.shift(ParseState::RegexRepeat),
                Token::Plus => self.shift(ParseState::RegexPlus),
                Token::Optional => self.shift(ParseState::RegexOptional),
                // concat has priority over '|' and  '&', so reduce here
                Token::Or => self.reduce_concat(),
                Token::And => self.reduce_concat(),
                Token::End => self.reduce_concat(),
            },
            ParseState::NegRegex => match self.lexer.peek() {
                // neg has top priority, but is ambiguous with posf-fix operators.
                Token::Zero => self.reduce_neg(),
                Token::Epsilon => self.reduce_neg(),
                Token::Regex(_) => self.reduce_neg(),
                Token::Neg => self.reduce_neg(),
                Token::Repeat => self.error(
                    "ambiguous regex: simultaneous use of '~' prefix and '*' postfix operator",
                ),
                Token::Plus => self.error(
                    "ambiguous regex: simultaneous use of '~' prefix and '+' postfix operator",
                ),
                Token::Optional => self.error(
                    "ambiguous regex: simultaneous use of '~' prefix and '?' postfix operator",
                ),
                Token::Or => self.reduce_neg(),
                Token::And => self.reduce_neg(),
                Token::End => self.reduce_neg(),
            },
            ParseState::RegexOrRegex => match self.lexer.peek() {
                // or has lowest priority, so shift in any case
                Token::Zero => self.shift(ParseState::Zero),
                Token::Epsilon => self.shift(ParseState::Epsilon),
                Token::Regex(_) => self.shift(ParseState::Symbol),
                Token::Neg => self.shift(ParseState::Neg),
                Token::Repeat => self.shift(ParseState::RegexRepeat),
                Token::Plus => self.shift(ParseState::RegexPlus),
                Token::Optional => self.shift(ParseState::RegexOptional),
                // or is left-associative, so reduce eagerly
                Token::Or => self.reduce_or(),
                Token::And => self.shift(ParseState::RegexAnd),
                Token::End => self.reduce_or(),
            },
            ParseState::RegexAndRegex => match self.lexer.peek() {
                // '&' has priority over '|' only, so shift in any other case
                Token::Zero => self.shift(ParseState::Zero),
                Token::Epsilon => self.shift(ParseState::Epsilon),
                Token::Regex(_) => self.shift(ParseState::Symbol),
                Token::Neg => self.shift(ParseState::Neg),
                Token::Repeat => self.shift(ParseState::RegexRepeat),
                Token::Plus => self.shift(ParseState::RegexPlus),
                Token::Optional => self.shift(ParseState::RegexOptional),
                // has priority over '|'
                Token::Or => self.reduce_and(),
                // and is left-recursive, so reduce eagerly
                Token::And => self.reduce_and(),
                Token::End => self.reduce_and(),
            },
        }
    }

    /// Extracts parsing result from the stack.
    fn finalize(mut self) -> syn::Result<Regex> {
        let regex = self.pop_stack_regex()?;
        if self.parse_stack.is_empty() {
            Ok(regex.deref().clone())
        } else {
            self.internal_error("residual input after parsing finished.")
        }
    }

    /// Entry point: parses the `input` to a [`Regex`].
    pub fn parse_regex(input: ParseStream) -> syn::Result<Regex> {
        let mut parser = Parser::init(input)?;
        let mut accept = false;
        while !accept {
            accept = parser.step()?
        }
        parser.finalize()
    }
}

impl Parse for Regex {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Parser::parse_regex(input)
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
