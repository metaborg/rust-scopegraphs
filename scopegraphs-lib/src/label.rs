/// Trait that allows iterating over all labels.
pub trait Label {
    fn iter() -> impl Iterator<Item = Self>
    where
        Self: Sized;

    fn iter_ref() -> impl Iterator<Item = &'static Self>
    where
        Self: Sized + 'static;
}

#[macro_export]
macro_rules! query_regex {
    ($label_type:path: $($regex:tt)*) => {
        {
            scopegraphs_macros::compile_regex!(type QueryRegex<$label_type> = $($regex)*);
            QueryRegex::new()
        }
    };
}

pub use query_regex;
