use std::ops::{Deref, DerefMut};
use std::{
    future::Future,
    pin::Pin,
    task::{Context, Poll},
};

pub struct FutureWrapper<'fut, T>(pub Box<dyn Future<Output = T> + 'fut>);

impl<'fut, T> Deref for FutureWrapper<'fut, T> {
    type Target = dyn Future<Output = T> + 'fut;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl<'fut, T> DerefMut for FutureWrapper<'fut, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.deref_mut()
    }
}

impl<'fut, T> Future for FutureWrapper<'fut, T> {
    type Output = T;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let p: Pin<&mut dyn Future<Output = T>> = self.as_mut();
        p.poll(cx)
    }
}
