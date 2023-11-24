use crate::Automaton;
use std::fmt::Display;
use std::io;
use std::io::Write;

impl<L> Automaton<L> 
    where L : Display
{
    /// Create a graphviz dot file from a compiled regex.
    pub fn output_dot(&self, w: &mut impl Write) -> io::Result<()> {
        writeln!(w, "digraph {{")?;

        for (src, state) in self.states.iter().enumerate() {
            let attrs = if state.is_accepting() {
                ", shape=doublecircle"
            } else {
                ", shape=circle"
            };
            writeln!(w, "node[label=\"{}\"{attrs}]; {src};", state.regex)?;
        }

        for (src, state) in self.states.iter().enumerate() {
            let all_default = state
                .transition_table
                .iter()
                .all(|(_, x)| x == &state.default_transition);
            if all_default {
                writeln!(w, "{src} -> {} [label=\"_\"];", state.default_transition)?;
            } else {
                for (sym, tgt) in &state.transition_table {
                    writeln!(w, "{src} -> {tgt} [label=\"{sym}\"];")?;
                }
            }
        }

        writeln!(w, "}}")?;

        Ok(())
    }
}
