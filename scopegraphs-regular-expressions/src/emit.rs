use crate::compile::MatchState;
use crate::Automaton;
use proc_macro2::{Ident, TokenStream};
use quote::{quote, TokenStreamExt};
use syn::Type;

impl Automaton {
    /// Convert this compiled regex into rust code that accepts this regular expression.
    /// `name` is the name of the type that is emitted, and `alphabet` is the type of symbols
    /// that the machine should accept.
    pub fn emit(&self, name: &Ident, alphabet: &Type, errors: Vec<syn::Error>) -> TokenStream {
        let Self {
            states, initial, ..
        } = self;

        let ids: Vec<_> = (0..states.len()).collect();
        let arms: Vec<_> = states
            .iter()
            .map(|s| {
                let options: Vec<_> = s.transition_table.keys().collect();
                let default_transition = s.default_transition;
                let new_states: Vec<_> = options
                    .iter()
                    .flat_map(|x| s.transition_table.get(*x))
                    .copied()
                    .collect();
                let matchers: Vec<_> = options.into_iter().map(|i| i.name.clone()).collect();

                quote!(
                    match token {
                        #(
                            #alphabet::#matchers => {self.state = #new_states;}
                        ),*
                        _ => {self.state = #default_transition;}
                    }

                )
            })
            .collect();

        let finals: Vec<_> = states.iter().map(|i| i.is_final).collect();
        let accepting: Vec<_> = states.iter().map(MatchState::is_accepting).collect();
        let compile_errors: TokenStream = errors
            .iter()
            .flat_map(syn::Error::to_compile_error)
            .collect();

        let mut result = quote!(
            #[derive(Clone)]
            struct #name {
                state: usize,
            }

            impl #name {
                fn new() -> Self { Self {state: #initial} }
            }

            impl scopegraphs::RegexMatcher<#alphabet> for #name {
                fn step(&mut self, token: #alphabet) {
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
            }

            impl scopegraphs::RegexMatcher<&#alphabet> for #name {
                fn step(&mut self, token: &#alphabet) {
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
            }
        );
        result.append_all(compile_errors);
        result
    }
}
