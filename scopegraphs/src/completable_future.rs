//! Copied and adapted from https://crates.io/crates/completable_future (due to dependency mismatch)
//!
//! # Completable Future
//!
//! Similar to Java's CompletableFuture, this crate provides a simple
//! future that can be completed and properly notified from elsewhere other
//! than the executor of the future. It is sutable for some blocking
//! tasks that could block the executor if we use a future directly in
//! an executor.
//!
//! A CompletableFuture is still a future and has all the combinators that
//! you can use to chain logic working on the result or the error. Also,
//! unlike Java and inherited from Rust's poll model future, some executor
//! needs to execute the CompletableFuture in order to get the result; the
//! thread or code that completes (or errors) the future will not execute
//! the logic chained after the future.
//!
//! The CompletableFuture uses Arc and Mutex to synchronize poll and completion,
//! so there's overhead for using it.
//!
//! # Example
//! ```
//! extern crate futures;
//! extern crate completable_future;
//!
//! use futures::prelude::*;
//! use futures::executor::block_on;
//! use std::thread::spawn;
//! use std::thread::sleep;
//! use std::time::Duration;
//! use completable_future::CompletableFuture;
//!
//! fn main() {
//!     let fut1 = CompletableFuture::<String, ()>::new();
//!     // we will give the signal to some worker for it to complete
//!     let mut signal = fut1.signal();
//!     let fut2 = fut1.and_then(|s| {
//!         // this will come from whoever completes the future
//!         println!("in fut2: {}", s);
//!         Ok("this comes from fut2".to_string())
//!     });
//!     
//!     let j = spawn(move || {
//!         println!("waiter thread: I'm going to block on fut2");
//!         let ret = block_on(fut2).unwrap();
//!         println!("waiter thread: fut2 completed with message -- {}", ret);
//!     });
//!     
//!     spawn(move || {
//!         println!("worker thread: going to block for 1000 ms");
//!         sleep(Duration::from_millis(1000));
//!         signal.complete("this comes from fut1".to_string());
//!         println!("worker thread: completed fut1");
//!     });
//!     
//!     j.join().unwrap();
//! }
//! ```

use futures::future::Future;
use futures::task::{AtomicWaker, Context, Waker};
use std::mem;
use std::pin::Pin;
use std::sync::{Arc, Mutex};
use std::task::Poll;

enum WakerWrapper {
    Registered(AtomicWaker),
    NotRegistered,
}

impl WakerWrapper {
    fn register(&mut self, waker: &Waker) {
        match self {
            &mut WakerWrapper::Registered(ref _dont_care) => (),
            &mut WakerWrapper::NotRegistered => {
                let w = AtomicWaker::new();
                w.register(waker);
                *self = WakerWrapper::Registered(w)
            }
        }
    }

    fn wake(&self) {
        match self {
            &WakerWrapper::Registered(ref w) => w.wake(),
            &WakerWrapper::NotRegistered => (),
        };
    }
}

enum FutureState<V> {
    Pending,
    Completed(V),
    Taken,
}

impl<V> FutureState<V> {
    fn swap(&mut self, new_val: FutureState<V>) -> FutureState<V> {
        mem::replace(self, new_val)
    }

    fn unwrap_val(&mut self) -> V {
        match self.swap(FutureState::Taken) {
            FutureState::Completed(val) => val,
            _ => panic!("cannot unwrap because my state is not completed"),
        }
    }
}

/// the state of the future; reference counted
struct SignalInternal<V> {
    waker: WakerWrapper,
    state: FutureState<V>,
}

/// A handle to the future state. When you create a completable future,
/// you should also create a signal that somebody can use to complete
/// the future.
#[derive(Clone)]
pub struct CompletableFutureSignal<V> {
    internal: Arc<Mutex<SignalInternal<V>>>,
}

impl<V> CompletableFutureSignal<V> {
    fn mutate_self(&mut self, new_state: FutureState<V>) -> bool {
        let mut internal = self.internal.lock().unwrap();
        match internal.state {
            FutureState::Pending => {
                internal.state.swap(new_state);
                internal.waker.wake();
                true
            }
            _ => false,
        }
    }

    /// Complete the associated CompletableFuture. This method
    /// can be called safely across multiple threads multiple times,
    /// but only the winning call would mutate the future; other calls
    /// will be rendered noop.
    ///
    /// Returns whether the call successfully mutates the future.
    pub fn complete(&mut self, value: V) -> bool {
        self.mutate_self(FutureState::Completed(value))
    }
}

/// A CompletableFuture is a future that you can expect a result (or error)
/// from and chain logic on. You will need some executor to actively poll
/// the result. Executors provided by the futures crate are usually good
/// enough for common situations.
///
/// If you use a custom executor, be careful that don't poll the CompletableFuture
/// after it has already completed (or errored) in previous polls. Doing so
/// will panic your executor.
pub struct CompletableFuture<V> {
    internal: Arc<Mutex<SignalInternal<V>>>,
}

impl<V> CompletableFuture<V> {
    /// Construct a CompletableFuture.
    pub fn new() -> CompletableFuture<V> {
        CompletableFuture {
            internal: Arc::new(Mutex::new(SignalInternal {
                waker: WakerWrapper::NotRegistered,
                state: FutureState::Pending,
            })),
        }
    }

    /// Construct a CompletableFuture that's already completed
    /// with the value provided.
    pub fn completed(val: V) -> CompletableFuture<V> {
        CompletableFuture {
            internal: Arc::new(Mutex::new(SignalInternal {
                waker: WakerWrapper::NotRegistered,
                state: FutureState::Completed(val),
            })),
        }
    }

    /// Get a CompletableFutureSignal that can be used to complete
    /// or error this CompletableFuture.
    pub fn signal(&self) -> CompletableFutureSignal<V> {
        CompletableFutureSignal {
            internal: self.internal.clone(),
        }
    }
}

impl<V> Future for CompletableFuture<V> {
    type Output = V;

    fn poll(self: Pin<&mut Self>, ctx: &mut Context) -> Poll<Self::Output> {
        let mut signal = self.internal.lock().unwrap();
        signal.waker.register(ctx.waker());

        let state = &mut signal.state;
        match state {
            FutureState::Pending => Poll::Pending,
            FutureState::Taken => {
                panic!("bug: the value has been taken, yet I'm still polled again")
            }
            FutureState::Completed(_) => Poll::Ready(state.unwrap_val()),
        }
    }
}
