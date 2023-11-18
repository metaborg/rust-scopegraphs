use crate::CompiledRegex;
use std::io;
use std::io::Write;

impl CompiledRegex {
    pub fn output_dot(&self, w: &mut impl Write) -> io::Result<()> {
        writeln!(w, "digraph {{")?;

        for (src, state) in &self.states {
            let attrs = if state.is_accepting() {
                ", shape=doublecircle"
            } else {
                ", shape=circle"
            };
            writeln!(w, "node[label=\"{}\"{attrs}]; {src};", state.regex)?;
        }

        for (src, state) in &self.states {
            for (sym, tgt) in &state.transition_table {
                writeln!(w, "{src} -> {tgt} [label=\"{sym}\"];")?;
            }
        }

        writeln!(w, "}}")?;

        Ok(())
    }
}
