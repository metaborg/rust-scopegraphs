use crate::resolve::{DataOrder, Env, ResolvedPath};
use std::fmt::Debug;
use std::hash::Hash;

pub trait EnvContainer<'sg, LABEL: 'sg, DATA: 'sg>: From<Env<'sg, LABEL, DATA>> {
    fn empty() -> Self;

    // has functional interface (-> Self) to accommodate Result/Option implementations.
    // e.g., `&mut self` cannot be changed from `Ok(_)` to `Err(_)`
    // implementations that use the interior mutability should return `self`
    fn lift_merge(self, other: Self) -> Self;

    fn lift_shadow<DO: DataOrder<DATA>>(self, sub_env: Self, data_order: DO) -> Self;
}

impl<'sg, LABEL, DATA: Debug> EnvContainer<'sg, LABEL, DATA> for Env<'sg, LABEL, DATA>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Eq,
{
    fn empty() -> Self {
        Env::default()
    }

    fn lift_merge(mut self, other: Self) -> Self {
        for path in other.into_iter() {
            self.insert(path)
        }
        self
    }

    fn lift_shadow<DO: DataOrder<DATA>>(mut self, sub_env: Self, data_order: DO) -> Self {
        let filtered_env = sub_env
            .into_iter()
            .filter(|p1| {
                if let Some(p2) = self.iter().find(|p2| data_order(p1.data, p2.data)) {
                    log::info!(
                        "Discarding {:?} in {:?}; shadowed by {:?} in {:?}",
                        p1.data,
                        p1.path.target(),
                        p2.data,
                        p2.path.target()
                    );
                    false
                } else {
                    true
                }
            })
            .collect::<Vec<_>>();
        for path in filtered_env {
            self.insert(path)
        }
        self
    }
}

impl<'sg, LABEL: 'sg, DATA: 'sg, EC: EnvContainer<'sg, LABEL, DATA>, E> From<Env<'sg, LABEL, DATA>>
    for Result<EC, E>
{
    fn from(value: Env<'sg, LABEL, DATA>) -> Self {
        Ok(EC::from(value))
    }
}

impl<'sg, LABEL: 'sg, DATA: 'sg, EC: EnvContainer<'sg, LABEL, DATA>, E>
    EnvContainer<'sg, LABEL, DATA> for Result<EC, E>
{
    fn empty() -> Self {
        Ok(EC::empty())
    }

    fn lift_merge(self, other: Self) -> Self {
        self.and_then(|ec1| other.map(|ec2| ec1.lift_merge(ec2)))
    }

    fn lift_shadow<DO: DataOrder<DATA>>(self, sub_env: Self, data_order: DO) -> Self {
        self.and_then(|ec1| sub_env.map(|ec2| ec1.lift_shadow(ec2, data_order)))
    }
}
