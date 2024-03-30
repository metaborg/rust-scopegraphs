pub mod completeness;
pub mod containers;
pub mod label;
pub mod resolve;

mod scopegraph;
pub use scopegraph::*;

#[cfg(test)]
mod test {
    use super::{Scope, ScopeGraph};

    #[test]
    fn test_create_scope() {
        let mut sg: ScopeGraph<usize, usize, _> = unsafe { ScopeGraph::raw() };
        let scope = sg.add_scope(42);
        assert_eq!(42, *sg.get_data(scope));
    }

    #[test]
    fn test_create_two_scopes() {
        let mut sg: ScopeGraph<usize, usize, _> = unsafe { ScopeGraph::raw() };

        let s1 = sg.add_scope(1);
        let s2 = sg.add_scope(2);

        assert_eq!(1, *sg.get_data(s1));
        assert_eq!(2, *sg.get_data(s2));
    }

    #[test]
    fn test_create_edge() {
        let mut sg: ScopeGraph<usize, usize, _> = unsafe { ScopeGraph::raw() };

        let s1 = sg.add_scope(1);
        let s2 = sg.add_scope(2);

        sg.add_edge(s1, 1, s2);

        assert_eq!(vec![s2], sg.get_edges(s1, 1));
        assert_eq!(Vec::<Scope>::new(), sg.get_edges(s1, 2));
    }

    #[test]
    fn test_create_edges() {
        let mut sg: ScopeGraph<usize, usize, _> = unsafe { ScopeGraph::raw() };

        let s1 = sg.add_scope(1);
        let s2 = sg.add_scope(2);
        let s3 = sg.add_scope(3);

        sg.add_edge(s1, 1, s2);
        sg.add_edge(s1, 1, s3);

        assert!(sg.get_edges(s1, 1).contains(&s2));
        assert!(sg.get_edges(s1, 1).contains(&s3));

        assert_eq!(Vec::<Scope>::new(), sg.get_edges(s1, 2));
    }
}
