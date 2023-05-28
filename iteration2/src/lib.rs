use std::error::Error;

type BoxError = Box<dyn Error + Send + Sync + 'static>;

pub trait Io {
    type Success;
}

pub trait IoExt: Io + Sized {
    fn flat_map<R: Io, F: FnOnce(Self::Success) -> R>(self, f: F) -> FlatMap<Self, F>;

    fn map<S, F: FnOnce(Self::Success) -> S>(self, f: F) -> Map<Self, F>;

    fn recover<U: Io<Success = Self::Success>, F: FnOnce(BoxError) -> U>(
        self,
        f: F,
    ) -> Recover<Self, F>;
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

    fn recover<U: Io<Success = T::Success>, F: FnOnce(BoxError) -> U>(
        self,
        f: F,
    ) -> Recover<Self, F> {
        Recover {
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

pub struct ToyIoFail<E>(E);

impl<E> ToyIoFail<E> {
    pub fn fail(e: E) -> Self {
        Self(e)
    }
}

impl<E> Io for ToyIoFail<E> {
    type Success = Never;
}

impl<E: Error + Send + Sync + 'static> Run for ToyIoFail<E> {
    fn eval(self) -> Result<Self::Success, BoxError> {
        Err(Box::new(self.0))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Never {}

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
        (self.func)(self.inner.eval()?).eval()
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
        Ok((self.func)(self.inner.eval()?))
    }
}

#[must_use]
pub struct Recover<T, F> {
    inner: T,
    func: F,
}

impl<T, S: Io<Success = T>, U: Io<Success = T>, F: FnOnce(BoxError) -> U> Io for Recover<S, F> {
    type Success = T;
}

impl<T, S: Run<Success = T>, U: Run<Success = T>, F: FnOnce(BoxError) -> U> Run for Recover<S, F> {
    fn eval(self) -> Result<Self::Success, BoxError> {
        match self.inner.eval() {
            Ok(x) => Ok(x),
            Err(e) => (self.func)(e).eval(),
        }
    }
}

pub fn unsafe_run_sync<R: Run>(r: R) -> Result<R::Success, Box<dyn Error + Send + Sync + 'static>> {
    r.eval()
}

#[cfg(test)]
mod test {
    use std::{error::Error, fmt::Display};

    use super::*;

    #[derive(Debug)]
    struct StrError(&'static str);

    impl Display for StrError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl Error for StrError {}

    #[test]
    fn fail_and_recover() {
        let effect = ToyIo::effect(|| println!("running first effect"))
            .flat_map(|_| ToyIoFail::fail(StrError("some error")))
            .flat_map(|_| ToyIo::effect(|| println!("second effect - will not run")))
            .recover(|e| ToyIo::effect(move || println!("recovered from failure: {e}")));

        unsafe_run_sync(effect).expect("cannot execute");
    }
}
