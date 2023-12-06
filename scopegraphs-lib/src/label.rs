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
    ($label_type:path[$($regex:tt)*]) => {
        {
            scopegraphs_macros::compile_regex!(type ___QueryRegex___<$label_type> = $($regex)*);
            ___QueryRegex___::new()
        }
    };
}

pub use query_regex;
