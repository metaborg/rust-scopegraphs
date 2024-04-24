/// Represents types that can be used as labels on [`scopegraph edges`](crate::concepts::edges).
pub trait Label {
    /// Iterate over all possible labels that this label type could be.
    fn iter() -> impl Iterator<Item = Self>
    where
        Self: Sized;

    /// Iterate over all possible labels that this label type could be, by reference.
    fn iter_ref() -> impl Iterator<Item = &'static Self>
    where
        Self: Sized + 'static;
}

/// `query_regex` is a wrapper around [`compile_regex`](crate::compile_regex) that should be used
/// when you're using the resulting regex only once, inline. This is often the case in
/// [scope graph queries](crate::resolve::Query).
///
/// [`compile_regex`](crate::compile_regex) generates a type, which when instantiated can match
/// a string. This macro avoids the need to instantiate, and combines the compiling and instantiation.
#[macro_export]
macro_rules! query_regex {
    ($label_type:path: $($regex:tt)*) => {
        {
            $crate::compile_regex!(type QueryRegex<$label_type> = $($regex)*);
            QueryRegex::new()
        }
    };
}
