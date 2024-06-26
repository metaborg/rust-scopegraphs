//! Contains the [`DynamicMatcher`], a regex matcher that can be  constructed at runtime.
//! It's less efficient that a compiled regex created through the
//! [`compile_regex`](scopegraphs::compile_regex) macro, but has the advantage that the regular
//! expression that is used does not need to be known at compile time.

use crate::compile::{MatchState, StateID};
use crate::{Automaton, RegexMatcher};

/// Matched regular expressions that are compiled at runtime.
#[derive(Clone)]
pub struct DynamicMatcher<'a> {
    compiled_regex: &'a Automaton,
    current_state: StateID,
}

impl Automaton {
    /// Instantiate a (dynamic) matcher of this automaton.
    ///
    /// A matcher holds, in addition to the state machine,
    /// also information about which state the machine is currently in.
    /// This current state can then be updated by taking transitions using
    /// [`step`](RegexMatcher::step).
    pub fn matcher(&self) -> DynamicMatcher {
        DynamicMatcher {
            compiled_regex: self,
            current_state: self.initial,
        }
    }
}

impl<'a> RegexMatcher<&'a str> for DynamicMatcher<'_> {
    fn step(&mut self, inp: &'a str) {
        let current_state = &self.compiled_regex.states[self.current_state];
        self.current_state = current_state
            .string_transition_table
            .get(inp)
            .copied()
            .unwrap_or(current_state.default_transition);
    }

    fn is_final(&self) -> bool {
        self.compiled_regex
            .states
            .get(self.current_state)
            .map(|i| i.is_final)
            .unwrap_or_default()
    }

    fn is_accepting(&self) -> bool {
        self.compiled_regex
            .states
            .get(self.current_state)
            .map(MatchState::is_accepting)
            .unwrap_or_default()
    }
}

impl RegexMatcher<char> for DynamicMatcher<'_> {
    fn step(&mut self, inp: char) {
        let mut data = [0; 4];
        self.step(&*inp.encode_utf8(&mut data))
    }

    fn is_final(&self) -> bool {
        RegexMatcher::<&str>::is_final(self)
    }

    fn is_accepting(&self) -> bool {
        RegexMatcher::<&str>::is_accepting(self)
    }
}

#[cfg(test)]
mod tests {
    use crate::{parse_regex, RegexMatcher};

    #[test]
    fn test_dynamic_regex() {
        let r = parse_regex("a b*").unwrap();
        let c = r.compile();

        assert!(c.matcher().accepts("a".chars()));
        assert!(c.matcher().accepts("ab".chars()));
        assert!(c.matcher().accepts("abbbbb".chars()));
        assert!(!c.matcher().accepts("ba".chars()));
        assert!(!c.matcher().accepts("".chars()));
    }
}
