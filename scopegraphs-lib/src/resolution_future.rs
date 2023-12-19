use std::{
    future::Future,
    pin::Pin,
    task::{Context, Poll},
};

pub enum ResolutionFuture<'fut, T> {
    Ready { value: T },
    PollFn { poll: PollMut<'fut, T> },
}

type PollMut<'fut, T> = Box<dyn FnMut(&Context) -> Poll<T> + 'fut>;

impl<'fut, T> Future for ResolutionFuture<'fut, T>
where
    T: Clone + Unpin,
{
    type Output = T;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match Pin::into_inner(self) {
            ResolutionFuture::Ready { value } => Poll::Ready(value.clone()),
            ResolutionFuture::PollFn { poll } => (*poll)(cx),
        }
    }
}

impl<'fut, T> From<T> for ResolutionFuture<'fut, T> {
    fn from(value: T) -> Self {
        ResolutionFuture::Ready { value }
    }
}

impl<'fut, T: Clone> ResolutionFuture<'fut, T> {
    pub(crate) fn flat_map(&'fut mut self, map: &'fut mut (impl FnMut(&T) -> Self + 'fut)) -> Self {
        match self {
            // outer value is ready, return mapping of that value
            ResolutionFuture::Ready { value } => map(value),
            // outer value is not ready, return `Poll` that peeks `self`, and maps it once there is a result.
            ResolutionFuture::PollFn { poll } => {
                // Return new polling future
                ResolutionFuture::PollFn {
                    poll: Box::new(|cx| match (*poll)(cx) {
                        // that when `self` is ready
                        Poll::Ready(value) => {
                            // applies the mapping
                            match map(&value) {
                                // and returns the mapped value when it is ready
                                ResolutionFuture::Ready { value } => Poll::Ready(value.clone()),
                                ResolutionFuture::PollFn { mut poll } => (*poll)(cx),
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
