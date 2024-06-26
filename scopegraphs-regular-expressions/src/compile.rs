use crate::regex::Symbol;
use crate::Regex;
#[cfg(feature = "dynamic")]
use quote::quote;
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;

pub type StateID = usize;

#[derive(Clone)]
pub(crate) struct MatchState {
    pub is_final: bool,
    nullable: bool,
    pub transition_table: HashMap<Rc<Symbol>, StateID>,
    #[cfg(feature = "dynamic")]
    pub string_transition_table: HashMap<String, StateID>,
    pub default_transition: StateID,
    pub regex: Rc<Regex>,
}

impl MatchState {
    pub fn is_accepting(&self) -> bool {
        self.nullable
    }
}

/// A regex automaton is a compiled regular expression.
///
/// It represents the state machine corresponding to the [`Regex`].
/// This struct can either be turned into a
/// * [`DynamicMatcher`](crate::dynamic::DynamicMatcher) to match on a regex that was compiled at runtime.
/// * An implementation of [`RegexMatcher`](crate::RegexMatcher) generated using [emit](Automaton::emit).
///
/// This function can be called at compile time (through the `compile_regex!` macro) and it
/// emits the Rust code that can match the regular expression.
///
/// You can create an automaton using [`Regex::compile`]
#[derive(Clone)]
pub struct Automaton {
    pub(crate) states: Vec<MatchState>,
    pub(crate) initial: StateID,
}

pub struct AlphabetOrder {
    order: HashMap<Rc<Symbol>, i64>,
}

impl AlphabetOrder {
    pub fn new(alphabet: &HashSet<Rc<Symbol>>) -> Self {
        Self {
            order: alphabet.iter().cloned().zip(0..).collect(),
        }
    }

    pub fn get(&self, symbol: &Rc<Symbol>) -> i64 {
        *self.order.get(symbol).expect("not in alphabet")
    }
}

type State = Rc<Regex>;

struct RegexCompiler {
    alphabet: HashSet<Rc<Symbol>>,
    regex: Rc<Regex>,
    state_transitions: HashMap<State, HashMap<Rc<Symbol>, State>>,
    default_transitions: HashMap<State, State>,
    reverse_transitions: HashMap<State, HashSet<State>>,
    alphabet_order: AlphabetOrder,
}

impl RegexCompiler {
    fn new(regex: Regex) -> Self {
        let alphabet = regex.alphabet();

        Self {
            alphabet_order: AlphabetOrder::new(&alphabet),
            alphabet,
            regex: Rc::new(regex),
            state_transitions: Default::default(),
            default_transitions: Default::default(),
            reverse_transitions: Default::default(),
        }
    }

    fn create_transitions_step(&mut self, state: State, work_list: &mut VecDeque<State>) {
        // if we've already seen this state, we are done instantly
        if self.state_transitions.contains_key(&state) {
            return;
        }

        let mut transitions = HashMap::new();

        // find all reachable states from this state
        for symbol in &self.alphabet {
            let next_state = state.derive(Some(symbol)).normalize(&self.alphabet_order);

            // put it in reverse states
            self.reverse_transitions
                .entry(next_state.clone())
                .or_default()
                .insert(state.clone());
            // add the transition
            transitions.insert(symbol.clone(), next_state.clone());

            // add to the work list
            work_list.push_back(next_state);
        }

        // now do exactly the same we did but for the default
        self.step_default_state(&state, work_list);

        self.state_transitions.insert(state, transitions);
    }

    fn step_default_state(&mut self, state: &State, work_list: &mut VecDeque<State>) {
        let default_state = state.derive(None).normalize(&self.alphabet_order);

        // put it in reverse states
        self.reverse_transitions
            .entry(default_state.clone())
            .or_default()
            .insert(state.clone());

        // add the transition (default)
        self.default_transitions
            .insert(state.clone(), default_state.clone());

        // add to the work list
        work_list.push_back(default_state);
    }

    fn create_transitions(&mut self) {
        let mut work_list = VecDeque::new();

        work_list.push_back(self.regex.clone());
        work_list.push_back(Rc::new(Regex::EmptySet));

        while let Some(state) = work_list.pop_front() {
            self.create_transitions_step(state, &mut work_list);
        }
    }

    fn find_non_final(&self) -> HashSet<&State> {
        let mut work_list = VecDeque::new();
        for state in self.state_transitions.keys() {
            if state.is_nullable() {
                work_list.push_back(state)
            }
        }

        let mut visited = HashSet::new();
        let mut non_final = HashSet::new();
        while let Some(state) = work_list.pop_front() {
            if visited.insert(state) {
                for next_state in self.reverse_transitions.get(state).into_iter().flatten() {
                    non_final.insert(next_state);
                    work_list.push_back(next_state);
                }
            }
        }
        non_final
    }

    fn find_state_ids(&self) -> HashMap<&State, StateID> {
        let mut state_ids = HashMap::new();
        for (state_id_counter, state) in self.state_transitions.keys().enumerate() {
            state_ids.insert(state, state_id_counter);
        }
        state_ids
    }

    fn compile(mut self) -> Automaton {
        self.create_transitions();

        let non_final = self.find_non_final();
        let state_ids = self.find_state_ids();

        let mut match_states = HashMap::new();
        for (state, edges) in &self.state_transitions {
            let transition_table: HashMap<_, _> = edges
                .iter()
                .map(|(k, v)| (k.clone(), *state_ids.get(v).unwrap()))
                .collect();

            let non_final = non_final.contains(state);
            let nullable = state.is_nullable();

            match_states.insert(
                *state_ids.get(state).unwrap(),
                MatchState {
                    is_final: !non_final,
                    nullable,
                    #[cfg(feature = "dynamic")]
                    string_transition_table: transition_table
                        .iter()
                        .map(|(label, dst)| {
                            let path = &label.name;
                            (format!("{}", quote!(#path)), *dst)
                        })
                        .collect(),
                    transition_table,
                    default_transition: *state_ids
                        .get(self.default_transitions.get(state).unwrap())
                        .unwrap(),
                    regex: state.clone(),
                },
            );
        }

        let mut match_states = match_states.into_iter().collect::<Vec<_>>();
        match_states.sort_unstable_by_key(|x| x.0);

        #[cfg(debug_assertions)]
        if !match_states.is_empty() {
            let mut curr = match_states[0].0;
            assert_eq!(curr, 0);
            for &(i, _) in match_states.iter().skip(1) {
                assert_eq!(i, curr + 1);
                curr = i;
            }
        }

        let compiled = Automaton {
            initial: *state_ids.get(&self.regex).unwrap(),
            states: match_states.into_iter().map(|i| i.1).collect(),
        };

        compiled
    }
}

impl Regex {
    /// Turn a [`Regex`] into an [`Automaton`].
    ///
    /// A `Regex` represents only the syntax of a regular expression,
    /// while a compiled regex, an `Automaton` is more like a state machine.
    /// It can directly be used to actually match on the regular expression.
    pub fn compile(self) -> Automaton {
        let compiler = RegexCompiler::new(self);
        compiler.compile()
    }
}
