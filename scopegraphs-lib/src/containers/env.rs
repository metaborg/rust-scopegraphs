use crate::resolve::{Env, ResolvedPath};
use std::future::Future;
use std::hash::Hash;
use std::pin::Pin;
use std::rc::Rc;
use std::task::{Context, Poll};

/// Interface for environment containers that support the operations required for query resolution.
pub trait EnvContainer<'sg, LABEL: 'sg, DATA: 'sg>: From<Env<'sg, LABEL, DATA>> {
    /// Creates a new, container with an empty environment.
    fn empty() -> Self;

    /// Maps the current container to a new one, based a provided mapping of the underlying environment.
    fn flat_map(&self, map: impl FnOnce(&Env<'sg, LABEL, DATA>) -> Self) -> Self;
}

impl<'sg, LABEL, DATA> EnvContainer<'sg, LABEL, DATA> for Env<'sg, LABEL, DATA>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Eq,
{
    fn empty() -> Self {
        Self::new()
    }

    fn flat_map(&self, map: impl FnOnce(&Env<'sg, LABEL, DATA>) -> Self) -> Self {
        map(self)
    }
}

impl<'sg, LABEL, DATA> EnvContainer<'sg, LABEL, DATA> for Rc<Env<'sg, LABEL, DATA>>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Eq,
{
    fn empty() -> Self {
        Self::new(Env::empty())
    }

    fn flat_map(&self, map: impl FnOnce(&Env<'sg, LABEL, DATA>) -> Self) -> Self {
        map(self)
    }
}

// Implementations for Results
impl<'sg, LABEL: 'sg, DATA: 'sg, E> From<Env<'sg, LABEL, DATA>>
    for Result<Env<'sg, LABEL, DATA>, E>
{
    fn from(value: Env<'sg, LABEL, DATA>) -> Self {
        Ok(value)
    }
}

impl<'sg, LABEL: 'sg, DATA: 'sg, E> EnvContainer<'sg, LABEL, DATA>
    for Result<Env<'sg, LABEL, DATA>, E>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Eq,
    E: Clone,
{
    fn empty() -> Self {
        Ok(Env::empty())
    }

    fn flat_map(&self, map: impl FnOnce(&Env<'sg, LABEL, DATA>) -> Self) -> Self {
        match self {
            Ok(env) => map(env),
            Err(err) => Err(err.clone()),
        }
    }
}

enum EnvFuture<'sg, LABEL, DATA> {
    Ready {
        env: Env<'sg, LABEL, DATA>,
    },
    Poll {
        poll: Box<dyn FnMut(&Context) -> Poll<Env<'sg, LABEL, DATA>>>,
    },
}

impl<'sg, LABEL, DATA> Future for EnvFuture<'sg, LABEL, DATA>
where
    Env<'sg, LABEL, DATA>: Clone,
{
    type Output = Env<'sg, LABEL, DATA>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match Pin::into_inner(self) {
            EnvFuture::Ready { env } => Poll::Ready(env.clone()),
            EnvFuture::Poll { poll } => match (*poll)(cx) {
                Poll::Ready(env) => Poll::Ready(env.clone()),
                Poll::Pending => Poll::Pending,
            },
        }
    }
}

impl<'sg, LABEL, DATA> From<Env<'sg, LABEL, DATA>> for EnvFuture<'sg, LABEL, DATA> {
    fn from(value: Env<'sg, LABEL, DATA>) -> Self {
        EnvFuture::Ready { env: value }
    }
}

impl<LABEL, DATA> EnvFuture<'static, LABEL, DATA>
where
    Env<'static, LABEL, DATA>: Clone,
{
    fn empty() -> Self {
        EnvFuture::Ready { env: Env::new() }
    }

    fn flat_map(
        &mut self,
        mut map: impl FnMut(&Env<'static, LABEL, DATA>) -> Self + 'static,
    ) -> Self {
        match self {
            // outer environment is ready, return mapping of that environment
            EnvFuture::Ready { env } => map(env),
            // outer environment is not ready, return `Poll` that peeks `self`, and maps it once there is a result.
            EnvFuture::Poll { poll } => {
                // Return new polling future
                EnvFuture::Poll {
                    poll: Box::new(move |cx| match (*poll)(cx) {
                        // that when `self` is ready
                        Poll::Ready(env) => {
                            // applies the mapping
                            match map(&env) {
                                // and returns the mapped environment when it is ready
                                EnvFuture::Ready { env } => Poll::Ready(env.clone()),
                                EnvFuture::Poll { mut poll } => (*poll)(cx),
                            }
                        }
                        // .., or, when `self` is not ready, indicates it is pending
                        Poll::Pending => Poll::Pending,
                    }),
                }
            }
        }
    }
}
