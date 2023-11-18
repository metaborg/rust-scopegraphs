use crate::regex::Symbol;
use crate::Regex;
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;

pub type StateID = usize;

pub struct MatchState {
    non_final: bool,
    nullable: bool,
    empty: bool,
    pub transition_table: HashMap<Rc<Symbol>, StateID>,
    pub default_transition: StateID,
}

impl MatchState {
    pub fn is_final(&self) -> bool {
        !self.non_final
    }

    pub fn is_accepting(&self) -> bool {
        self.nullable
    }

    pub fn is_oblivion(&self) -> bool {
        self.empty
    }
}

pub struct CompiledRegex {
    pub regex: Rc<Regex>,
    pub states: HashMap<StateID, MatchState>,
    pub initial: StateID,
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
            let next_state = state
                .apply_symbol(Some(symbol))
                .normalize(&self.alphabet_order);

            // put it in reverse states
            self.reverse_transitions
                .entry(next_state.clone())
                .or_insert_with(HashSet::new)
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
        let default_state = state.apply_symbol(None).normalize(&self.alphabet_order);

        // put it in reverse states
        self.reverse_transitions
            .entry(default_state.clone())
            .or_insert_with(HashSet::new)
            .insert(state.clone());

        // add the transition (default)
        self.default_transitions
            .insert(state.clone(), default_state.clone());

        // add to the work list
        work_list.push_back(default_state);
    }

    fn create_transitions(&mut self) {
        let mut work_list = VecDeque::new();

        work_list.push_back(Rc::new(Regex::EmptySet));
        work_list.push_back(self.regex.clone());

        while let Some(state) = work_list.pop_front() {
            println!("{:?}", state);
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
        let mut state_id_counter = 0;
        for state in self.state_transitions.keys() {
            state_ids.insert(state, state_id_counter);
            state_id_counter += 1;
        }
        state_ids
    }

    fn find_nullable(&self) -> HashSet<&State> {
        self.state_transitions
            .keys()
            .filter(|x| x.is_nullable())
            .collect::<HashSet<_>>()
    }

    fn compile(mut self) -> CompiledRegex {
        self.create_transitions();

        let non_final = self.find_non_final();
        let nullable = self.find_nullable();
        let state_ids = self.find_state_ids();

        let mut match_states = HashMap::new();
        for (state, edges) in &self.state_transitions {
            let transition_table = edges
                .iter()
                .map(|(k, v)| (k.clone(), *state_ids.get(v).unwrap()))
                .collect();

            match_states.insert(
                *state_ids.get(state).unwrap(),
                MatchState {
                    non_final: non_final.contains(state),
                    nullable: nullable.contains(state),
                    empty: state.is_empty(),
                    transition_table,
                    default_transition: *state_ids
                        .get(self.default_transitions.get(state).unwrap())
                        .unwrap(),
                },
            );
        }

        CompiledRegex {
            initial: *state_ids.get(&self.regex).unwrap(),
            regex: self.regex,
            states: match_states,
        }
    }
}

impl Regex {
    pub fn compile(self) -> CompiledRegex {
        let compiler = RegexCompiler::new(self);
        compiler.compile()
    }
}
