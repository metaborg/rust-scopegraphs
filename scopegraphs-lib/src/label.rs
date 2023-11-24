pub trait Label {
    fn iter() -> impl Iterator<Item = Self>
    where
        Self: Sized;

    fn iter_ref() -> impl Iterator<Item = &'static Self>
    where
        Self: Sized + 'static;
}
