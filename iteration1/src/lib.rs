use std::error::Error;

type BoxError = Box<dyn Error + Send + Sync + 'static>;

pub trait Io {
    type Success;
}

pub trait IoExt: Io + Sized {
    fn flat_map<R: Io, F: FnOnce(Self::Success) -> R>(self, f: F) -> FlatMap<Self, F>;

    fn map<S, F: FnOnce(Self::Success) -> S>(self, f: F) -> Map<Self, F>;
}

impl<T: Io> IoExt for T {
    fn flat_map<R: Io, F: FnOnce(Self::Success) -> R>(self, f: F) -> FlatMap<T, F> {
        FlatMap {
            inner: self,
            func: f,
        }
    }

    fn map<S, F: FnOnce(Self::Success) -> S>(self, f: F) -> Map<T, F> {
        Map {
            inner: self,
            func: f,
        }
    }
}

pub trait Run: Io {
    fn eval(self) -> Result<Self::Success, BoxError>;
}

pub struct ToyIo<T>(T);

impl<T, F: FnOnce() -> T> ToyIo<F> {
    pub fn effect(f: F) -> Self {
        Self(f)
    }
}

impl<T, F: FnOnce() -> T> Io for ToyIo<F> {
    type Success = T;
}

impl<T, F: FnOnce() -> T> Run for ToyIo<F> {
    fn eval(self) -> Result<Self::Success, BoxError> {
        Ok(self.0())
    }
}

#[must_use]
pub struct FlatMap<T, F> {
    inner: T,
    func: F,
}

impl<T: Io, R: Io, F: FnOnce(T::Success) -> R> Io for FlatMap<T, F> {
    type Success = R::Success;
}

impl<T: Run, R: Run, F: FnOnce(T::Success) -> R> Run for FlatMap<T, F> {
    fn eval(self) -> Result<Self::Success, BoxError> {
        match self.inner.eval() {
            Ok(x) => (self.func)(x).eval(),
            Err(e) => Err(e),
        }
    }
}

#[must_use]
pub struct Map<T, F> {
    inner: T,
    func: F,
}

impl<T: Io, S, F: FnOnce(T::Success) -> S> Io for Map<T, F> {
    type Success = S;
}

impl<T: Run, S, F: FnOnce(T::Success) -> S> Run for Map<T, F> {
    fn eval(self) -> Result<Self::Success, BoxError> {
        match self.inner.eval() {
            Ok(x) => Ok((self.func)(x)),
            Err(e) => Err(e),
        }
    }
}

pub fn unsafe_run_sync<R: Run>(r: R) -> Result<R::Success, Box<dyn Error + Send + Sync + 'static>> {
    r.eval()
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

        unsafe_run_sync(effect).expect("cannot execute");
    }
}
