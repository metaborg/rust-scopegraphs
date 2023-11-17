pub trait Label: Copy + Eq {
    fn iter() -> impl Iterator<Item = Self>
    where
        Self: Sized;
}
