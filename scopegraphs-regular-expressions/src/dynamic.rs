//! Contains the [`DynamicMatcher`], a regex matcher that can be  constructed at runtime.
//! It's less efficient that a compiled regex created through the
//! [`compile_regex`](scopegraphs::compile_regex) macro, but has the advantage that the regular
//! expression that is used does not need to be known at compile time.

use crate::compile::StateID;
use crate::{Automaton, MatchState, RegexMatcher};

use std::hash::Hash;

pub struct DynamicMatcher<'a, L> {
    compiled_regex: &'a Automaton<L>,
    current_state: StateID,
}

impl<L> Automaton<L> {
    pub fn matcher(&self) -> DynamicMatcher<L> {
        DynamicMatcher {
            compiled_regex: self,
            current_state: self.initial,
        }
    }
}

impl<'a, L> RegexMatcher<&'a L> for DynamicMatcher<'_, L> 
    where L: Hash + PartialEq + Eq
{
    fn step(&mut self, inp: &'a L) {
        let current_state = &self.compiled_regex.states[self.current_state];
        self.current_state = current_state
            .transition_table
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

#[cfg(test)]
mod tests {
    use crate::{parse_regex, RegexMatcher, Regex, Automaton};

    #[test]
    fn test_dynamic_regex() {
        let r = parse_regex("a b*").unwrap();
        let c: Automaton<char> = r.compile().try_convert().unwrap();

        assert!(c.matcher().accepts("a".chars()));
        assert!(c.matcher().accepts("ab".chars()));
        assert!(c.matcher().accepts("abbbbb".chars()));
        assert!(!c.matcher().accepts("ba".chars()));
        assert!(!c.matcher().accepts("".chars()));
    }
}
