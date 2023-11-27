pub trait Label<'lbl>: 'lbl {
    fn iter() -> impl Iterator<Item = Self>
    where
        Self: Sized;

    fn iter_ref() -> impl Iterator<Item = &'lbl Self>
    where
        Self: Sized;
}
