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
                ""
            };
            writeln!(w, "node[label=\"{}\"{attrs}] id{src}", state.regex)?;

            for (sym, tgt) in &state.transition_table {
                writeln!(w, "id{src} -> id{tgt} [label=\"{sym}\"]")?;
            }
        }

        writeln!(w, "}}")?;

        Ok(())
    }
}
