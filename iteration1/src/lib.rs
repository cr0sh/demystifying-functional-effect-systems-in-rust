use std::error::Error;

type BoxError = Box<dyn Error + Send + Sync + 'static>;

pub trait Io {
    type Output;
}

pub trait IoExt: Io {
    type FlatMap<R: Io, F: FnOnce(Self::Output) -> R>: Io;
    fn flat_map<R: Io, F: FnOnce(Self::Output) -> R>(self, f: F) -> Self::FlatMap<R, F>;

    type Map<S, F: FnOnce(Self::Output) -> S>: Io;
    fn map<S, F: FnOnce(Self::Output) -> S>(self, f: F) -> Self::Map<S, F>;
}

impl<T: Io> IoExt for T {
    type FlatMap<R: Io, F: FnOnce(Self::Output) -> R> = FlatMap<T, F>;
    fn flat_map<R: Io, F: FnOnce(Self::Output) -> R>(self, f: F) -> Self::FlatMap<R, F> {
        FlatMap {
            inner: self,
            func: f,
        }
    }

    type Map<S, F: FnOnce(Self::Output) -> S> = Map<T, F>;
    fn map<S, F: FnOnce(Self::Output) -> S>(self, f: F) -> Self::Map<S, F> {
        Map {
            inner: self,
            func: f,
        }
    }
}

pub trait Run: Io {
    fn unsafe_run_sync(self) -> Result<Self::Output, BoxError>;
}

pub struct ToyIo<T>(T);

impl<T, F: FnOnce() -> T> ToyIo<F> {
    pub fn effect(f: F) -> Self {
        Self(f)
    }
}

impl<T, F: FnOnce() -> T> Io for ToyIo<F> {
    type Output = T;
}

impl<T, F: FnOnce() -> T> Run for ToyIo<F> {
    fn unsafe_run_sync(self) -> Result<Self::Output, BoxError> {
        Ok(self.0())
    }
}

#[must_use]
pub struct FlatMap<T, F> {
    inner: T,
    func: F,
}

impl<T: Io, R: Io, F: FnOnce(T::Output) -> R> Io for FlatMap<T, F> {
    type Output = R::Output;
}

impl<T: Run, R: Run, F: FnOnce(T::Output) -> R> Run for FlatMap<T, F> {
    fn unsafe_run_sync(self) -> Result<Self::Output, BoxError> {
        (self.func)(self.inner.unsafe_run_sync()?).unsafe_run_sync()
    }
}

#[must_use]
pub struct Map<T, F> {
    inner: T,
    func: F,
}

impl<T: Io, S, F: FnOnce(T::Output) -> S> Io for Map<T, F> {
    type Output = S;
}

impl<T: Run, S, F: FnOnce(T::Output) -> S> Run for Map<T, F> {
    fn unsafe_run_sync(self) -> Result<Self::Output, BoxError> {
        Ok((self.func)(self.inner.unsafe_run_sync()?))
    }
}

pub fn execute<R: Run>(r: R) -> Result<R::Output, Box<dyn Error + Send + Sync + 'static>> {
    r.unsafe_run_sync()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::{self, BufRead};

    #[test]
    fn program() {
        let _ = ToyIo::effect(|| println!("what's your name?"))
            .flat_map(|()| {
                ToyIo::effect(|| {
                    let mut stdin = io::stdin().lock();
                    let mut buf = String::new();
                    stdin.read_line(&mut buf).expect("cannot read");
                    buf
                })
            })
            .map(|name| ToyIo::effect(move || println!("hello, {name}!")));
        // .run();
    }

    #[test]
    fn sequence_effects() {
        let effect = ToyIo::effect(|| println!("running first effect"))
            .flat_map(|()| ToyIo::effect(|| println!("running second effect")));

        execute(effect).expect("cannot execute");
    }
}
