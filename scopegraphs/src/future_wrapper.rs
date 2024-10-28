//! Defines a cloneable pointer to a future.

use futures::future::Shared;
use futures::FutureExt;
use std::fmt::{Debug, Formatter};
use std::{
    future::Future,
    pin::Pin,
    task::{Context, Poll},
};

// FIXME: only expose futures on tests
/// Shared pointer to a future, useful to make functions parametric over the type of future they are invoked with.
pub struct FutureWrapper<'fut, T>(pub Shared<Pin<Box<dyn Future<Output = T> + 'fut>>>);

impl<T> Clone for FutureWrapper<'_, T> {
    fn clone(&self) -> Self {
        Self(Shared::clone(&self.0))
    }
}

impl<'fut, T: Clone> FutureWrapper<'fut, T> {
    /// Creates shared pointer to `f`.
    pub fn new(f: impl Future<Output = T> + 'fut) -> Self {
        let f: Pin<Box<dyn Future<Output = T> + 'fut>> = Box::pin(f);
        Self(f.shared())
    }
}

impl<T> Debug for FutureWrapper<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<future>")
    }
}

impl<T: Clone> Future for FutureWrapper<'_, T> {
    type Output = T;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Pin::new(&mut self.0).poll(cx)
    }
}
