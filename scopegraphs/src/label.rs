use std::array;

pub(crate) trait ArrayInit<T> {
    fn init_from_fn(init: impl FnMut() -> T) -> Self;
}

impl<T, const N: usize> ArrayInit<T> for [T; N] {
    fn init_from_fn(mut init: impl FnMut() -> T) -> Self {
        array::from_fn(|_| init())
    }
}

unsafe impl Label for () {
    type Array<T> = [T; 1];

    fn to_usize(&self) -> usize {
        0
    }

    fn iter() -> impl Iterator<Item = Self>
    where
        Self: Sized,
    {
        [()].into_iter()
    }

    fn iter_ref() -> impl Iterator<Item = &'static Self>
    where
        Self: Sized + 'static,
    {
        [&()].into_iter()
    }
}

/// Represents types that can be used as labels on [`scopegraph edges`](crate::concepts::edges).
///
/// Can, and should, be derived using the [`Label`](scopegraphs_macros::Label) derive macro.
///
/// # Safety
///
/// This trait is unsafe to implement by yourself, as code in this library depends on the fact
/// that [`to_usize`](Label::to_usize) returns a list of consecutive indices from 0 to [`NUM_LABELS`](Label::NUM_LABELS) - 1
pub unsafe trait Label: Eq + Copy {
    /// The type of an array of these labels, with one slot for each label type.
    /// NOTE: using an associated const doesn't work without full const generics
    #[allow(private_bounds)]
    type Array<T>: AsRef<[T]> + AsMut<[T]> + ArrayInit<T>;

    /// Convert any label to a unique usize, which m
    fn to_usize(&self) -> usize;

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
