use futures::future::Shared;
use futures::FutureExt;
use std::fmt::{Debug, Formatter};
use std::{
    future::Future,
    pin::Pin,
    task::{Context, Poll},
};

// TODO: fork futures and create our own shared future that can have a
//       ?Sized inner type (this should be possible)
pub struct FutureWrapper<'fut, T>(pub Shared<Pin<Box<dyn Future<Output = T> + 'fut>>>);

impl<'fut, T> Clone for FutureWrapper<'fut, T> {
    fn clone(&self) -> Self {
        Self(Shared::clone(&self.0))
    }
}

impl<'fut, T: Clone> FutureWrapper<'fut, T> {
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

impl<'fut, T: Clone> Future for FutureWrapper<'fut, T> {
    type Output = T;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Pin::new(&mut self.0).poll(cx)
    }
}
