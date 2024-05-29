use std::{fmt::Debug, hash::Hash};

use crate::{Label, Scope, ScopeGraph};

use super::ExplicitClose;

/// Represents the permission to extend a scope with
pub struct ScopeExt<'ext, 'storage, LABEL: Hash + Eq + Label, DATA> {
    // Bound on Label required for Drop implementation
    scope: Scope,
    label: LABEL,
    sg: &'ext ScopeGraph<'storage, LABEL, DATA, ExplicitClose<LABEL>>,
}

impl<'ext, 'storage, LABEL: Hash + Eq + Label, DATA> Drop
    for ScopeExt<'ext, 'storage, LABEL, DATA>
{
    fn drop(&mut self) {
        self.sg.close(self.scope, &self.label)
    }
}

impl<'ext, 'storage, LABEL: Hash + Eq + Label, DATA> ScopeExt<'ext, 'storage, LABEL, DATA> {
    /// This is an implementation detail of the [new_scope_with_ext!] macro and should not be called directly!
    #[doc(hidden)]
    pub unsafe fn init(
        scope: Scope,
        label: LABEL,
        sg: &'ext ScopeGraph<'storage, LABEL, DATA, ExplicitClose<LABEL>>,
    ) -> ScopeExt<'ext, 'storage, LABEL, DATA> {
        ScopeExt { scope, label, sg }
    }
}

/// Creates a scope (with some data if specified), and permission to extend it for each label specified in the label list argument.
///
/// TODO: Better documentation, examples.
#[macro_export]
macro_rules! new_scope_with_ext {
  ($sg:expr, $data:expr, [ $($lbl:expr),* ]) => {
    {
        // put initialized code in block
        let sg = $sg;       // evaluate scope graph expression
        let data = $data;   // evaluate data expression

        let scope = sg.add_scope_with(data, [$($lbl),*]);

        (scope $(, unsafe { ScopeExt::init(scope, $lbl, sg) } )*)
    }
  };

  ($sg:expr, [$($lbl:expr),* ]) => { new_scope_with_ext!($sg, Default::default(), [$($lbl),*]) };
}

impl<'ext, 'storage, LABEL: Hash + Eq + Label + Copy + Debug, DATA>
    ScopeGraph<'storage, LABEL, DATA, ExplicitClose<LABEL>>
{
    /// Adds a new edge to the target. The source and label are inferred from the `scope_ext` argument.
    pub fn ext_edge(&self, scope_ext: &ScopeExt<'ext, 'storage, LABEL, DATA>, target: Scope) {
        self.add_edge(scope_ext.scope, scope_ext.label, target)
            .expect("existence of ScopeExt instance guarantees safety");
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        completeness::ExplicitClose, completeness::ScopeExt, storage::Storage, Label, ScopeGraph,
    };

    use std::cmp::{Eq, PartialEq};

    pub mod scopegraphs {
        pub use crate::*;
    }

    #[derive(Debug, Hash, Label, PartialEq, Eq, Clone, Copy)]
    enum Lbl {
        Lbl1,
        Lbl2,
    }

    #[test]
    pub fn with_drop() {
        let storage = Storage::new();
        let scope_graph: ScopeGraph<Lbl, (), ExplicitClose<Lbl>> =
            ScopeGraph::new(&storage, ExplicitClose::default());

        let (s1, lbl12_ext) = new_scope_with_ext!(&scope_graph, (), [Lbl::Lbl2]);
        let (s2, lbl21_ext, lbl22_ext) = new_scope_with_ext!(&scope_graph, [Lbl::Lbl1, Lbl::Lbl2]);

        scope_graph.ext_edge(&lbl12_ext, s2);
        scope_graph.ext_edge(&lbl21_ext, s1);
        scope_graph.ext_edge(&lbl22_ext, s1);

        drop(lbl12_ext);
        drop(lbl21_ext);
        println!("{:?}", scope_graph);
        drop(lbl22_ext);
    }
}
