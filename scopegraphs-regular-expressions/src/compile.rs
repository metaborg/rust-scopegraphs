use crate::parse::Symbol;
use crate::Regex;
use std::borrow::Borrow;
use std::hash::Hash;
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;

pub type StateID = usize;

#[derive(Debug, Clone)]
pub struct MatchState<L> {
    pub is_final: bool,
    nullable: bool,
    pub transition_table: HashMap<Rc<L>, StateID>,
    // #[cfg(feature = "dynamic")]
    // pub string_transition_table: HashMap<String, StateID>,
    pub default_transition: StateID,
    pub regex: Rc<Regex<L>>,
}

impl<L> MatchState<L> {
    pub fn is_accepting(&self) -> bool {
        self.nullable
    }
}

impl<'a, L: 'a> MatchState<L> {
    pub fn convert<T: Hash + Eq + From<&'a L>>(&self) -> MatchState<T> {
        MatchState { 
            is_final: self.is_final,
            nullable: self.nullable,
            transition_table: self.transition_table.clone().into_iter().map(|(k, v)| (Rc::new(k.as_ref().into()), v)).collect(),
            default_transition: self.default_transition,
            regex: Rc::new(self.regex.convert()),
        }
    }
    pub fn try_convert<T: Hash + Eq + TryFrom<&'a L>>(&self) -> Result<MatchState<T>, T::Error>
        where T: std::fmt::Debug, T::Error : std::fmt::Debug + Clone
    {
        let trans_results: Vec<Result<(Rc<T>, StateID), T::Error>> = self
            .transition_table
            .clone()
            .into_iter()
            .map(|(l, st)| l.as_ref().try_into().map(|t| (Rc::new(t), st)))
            .collect();
        let err = trans_results.clone().into_iter().find(|s| s.is_err());

        match err {
            Some(err) => return Err(err.unwrap_err()),
            None => {
                Ok(MatchState { 
                    is_final: self.is_final,
                    nullable: self.nullable,
                    transition_table: trans_results.clone().into_iter().map(|r| r.unwrap()).collect(),
                    default_transition: self.default_transition,
                    regex: Rc::new(self.regex.try_convert()?),
                })
            },
        }
        
    }
}

pub struct Automaton<L> {
    pub regex: Rc<Regex<L>>,
    pub states: Vec<MatchState<L>>,
    pub initial: StateID,
}

impl<'a, L: 'a> Automaton<L> {
    pub fn convert<T: Hash + Eq + From<&'a L>>(&self) -> Automaton<T> {
        Automaton { 
            regex: Rc::new(self.regex.convert()), 
            states: self.states.iter().map(MatchState::convert).collect(), 
            initial: self.initial,
        }
    }
    pub fn try_convert<T: Hash + Eq + TryFrom<&'a L>>(&self) -> Result<Automaton<T>, T::Error>
        where L: Clone, T: std::fmt::Debug + Clone, T::Error : std::fmt::Debug + Clone
    {
        let state_results: Vec<_> = self.states.clone().into_iter().map(|st| st.try_convert()).collect();
        let err = state_results.clone().into_iter().find(|s| s.is_err());
        match err {
            Some(err) => return Err(err.unwrap_err()),
            None => {
                let states = state_results.into_iter().map(|s| s.unwrap()).collect();
                Ok(Automaton { 
                    regex: Rc::new(self.regex.try_convert()?), 
                    states, 
                    initial: self.initial,
                })
            },
        }
        
    }
}

pub struct AlphabetOrder<L> {
    order: HashMap<Rc<L>, i64>,
}

impl<L> AlphabetOrder<L> 
    where L: Hash + PartialEq + Eq 
{
    pub fn new(alphabet: &HashSet<Rc<L>>) -> Self {
        Self {
            order: alphabet.iter().cloned().zip(0..).collect(),
        }
    }

    pub fn get(&self, symbol: &Rc<L>) -> i64 {
        *self.order.get(symbol).expect("not in alphabet")
    }
}

type State<L> = Rc<Regex<L>>;

struct RegexCompiler<L> {
    alphabet: HashSet<Rc<L>>,
    regex: Rc<Regex<L>>,
    state_transitions: HashMap<State<L>, HashMap<Rc<L>, State<L>>>,
    default_transitions: HashMap<State<L>, State<L>>,
    reverse_transitions: HashMap<State<L>, HashSet<State<L>>>,
    alphabet_order: AlphabetOrder<L>,
}

impl<L> RegexCompiler<L> 
    where L: Hash + PartialEq + Eq + Borrow<L> 
{
    fn new(regex: Regex<L>) -> Self {
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

    fn create_transitions_step(&mut self, state: State<L>, work_list: &mut VecDeque<State<L>>) {
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

    fn step_default_state(&mut self, state: &State<L>, work_list: &mut VecDeque<State<L>>) {
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

    fn find_non_final(&self) -> HashSet<&State<L>> {
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

    fn find_state_ids(&self) -> HashMap<&State<L>, StateID> {
        let mut state_ids = HashMap::new();
        for (state_id_counter, state) in self.state_transitions.keys().enumerate() {
            state_ids.insert(state, state_id_counter);
        }
        state_ids
    }

    fn compile(mut self) -> Automaton<L> {
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
                    /* #[cfg(feature = "dynamic")]
                    string_transition_table: transition_table
                        .iter()
                        .map(|(label, dst)| {
                            let path = &label.name;
                            (format!("{}", quote!(#path)), *dst)
                        })
                        .collect(), */
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
            regex: self.regex,
            states: match_states.into_iter().map(|i| i.1).collect(),
        };

        compiled
    }
}

impl Regex<Symbol> {
    pub fn compile(self) -> Automaton<Symbol> {
        let compiler = RegexCompiler::new(self);
        compiler.compile()
    }
}
