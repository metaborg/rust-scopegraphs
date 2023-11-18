use crate::CompiledRegex;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

impl ToTokens for CompiledRegex {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            states, initial, ..
        } = self;

        let ids: Vec<_> = states.keys().collect();
        let arms: Vec<_> = ids
            .iter()
            .flat_map(|i| states.get(i))
            .map(|i| {
                let options: Vec<_> = i.transition_table.keys().collect();
                let default_transition = i.default_transition;
                let new_states: Vec<_> = options
                    .iter()
                    .flat_map(|x| i.transition_table.get(*x))
                    .map(|i| *i)
                    .collect();
                let matchers: Vec<_> = options.into_iter().map(|i| i.name.clone()).collect();

                quote!(
                    match token {
                        #(
                            #matchers => {self.state = #new_states;}
                        ),*
                        _ => {self.state = #default_transition;}
                    }

                )
            })
            .collect();

        let finals: Vec<_> = ids
            .iter()
            .flat_map(|i| states.get(i))
            .map(|i| i.is_final())
            .collect();

        let accepting: Vec<_> = ids
            .iter()
            .flat_map(|i| states.get(i))
            .map(|i| i.is_accepting())
            .collect();

        let oblivions: Vec<_> = ids
            .iter()
            .flat_map(|i| states.get(i))
            .map(|i| i.is_oblivion())
            .collect();

        tokens.extend(quote!(
            struct Machine {
                state: usize,
            }

            impl Machine {
                pub fn new() -> Self { Self {state: #initial} }
            }

            impl scopegraphs::RegexMatcher for Machine {
                type Alphabet = A;

                fn accept(&mut self, token: Self::Alphabet) {
                    match self.state {
                        #(
                            #ids => #arms
                        ),*
                        _ => unreachable!(),
                    }
                }

                fn is_final(&self) -> bool {
                    match self.state {
                        #(
                            #ids => {return #finals;}
                        ),*
                        _ => unreachable!(),
                    }
                }

                fn is_accepting(&self) -> bool {
                    match self.state {
                        #(
                            #ids => {return #accepting;}
                        ),*
                        _ => unreachable!(),
                    }
                }

                fn is_oblivion(&self) -> bool {
                    match self.state {
                        #(
                            #ids => {return #oblivions;}
                        ),*
                        _ => unreachable!(),
                    }
                }
            }
        ))
    }
}
