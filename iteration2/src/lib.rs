/// A trait that [`ToyIo<T, E>`] should implement.
pub trait Io {
    type Success;
    type Error;

    fn eval(self) -> Result<Self::Success, Self::Error>;
}

/// A 'toy' IO effect that can either succeed with `T` or fail with `E`.
pub enum ToyIo<T, E> {
    Effect(Box<dyn FnOnce() -> Result<T, E>>),
    Fail(E),
    // Should be boxed to 'erase' the return type of the inner effect
    FlatMap(Box<dyn FlatMap<Success = T, Error = E>>),
    Recover(Box<Self>, Box<dyn FnOnce(E) -> Self>),
}

impl<T: 'static, E: 'static> ToyIo<T, E> {
    /// Creates an effect which immediatly succeeds with the value given.
    pub fn succeed(x: T) -> Self {
        Self::Effect(Box::new(move || Ok(x)))
    }

    /// Creates a succeeding effect which executes the function given.
    pub fn effect(f: impl FnOnce() -> T + 'static) -> Self {
        Self::Effect(Box::new(move || Ok(f())))
    }

    /// Creates a failing effect with given value.
    pub fn fail(err: E) -> Self {
        Self::Fail(err)
    }

    /// Executes the effect given after this effect succeeds.
    ///
    /// The input fed to `f` is the result of this effect.
    pub fn flat_map<U: 'static>(self, f: impl FnOnce(T) -> ToyIo<U, E> + 'static) -> ToyIo<U, E> {
        ToyIo::FlatMap(Box::new(Some(FlatMapImpl {
            inner: self,
            func: f,
        })))
    }

    /// Recovers if this effect fails.
    ///
    /// The input fed to `f` is the error of this effect.
    pub fn recover(self, f: impl FnOnce(E) -> Self + 'static) -> Self {
        Self::Recover(Box::new(self), Box::new(f))
    }
}

impl<T, E> Io for ToyIo<T, E> {
    type Success = T;

    type Error = E;

    fn eval(self) -> Result<Self::Success, Self::Error> {
        match self {
            ToyIo::Effect(eff) => eff(),
            ToyIo::Fail(e) => Err(e),
            ToyIo::FlatMap(mut inner) => inner.mapped().eval(),
            ToyIo::Recover(inner, f) => match inner.eval() {
                Ok(x) => Ok(x),
                Err(e) => f(e).eval(),
            },
        }
    }
}

pub trait FlatMap {
    type Success;
    type Error;

    fn mapped(&mut self) -> ToyIo<Self::Success, Self::Error>;
}

struct FlatMapImpl<T, U, E, F: FnOnce(T) -> ToyIo<U, E>> {
    inner: ToyIo<T, E>,
    func: F,
}

impl<T, U, E, F: FnOnce(T) -> ToyIo<U, E>> FlatMap for Option<FlatMapImpl<T, U, E, F>> {
    type Success = U;
    type Error = E;

    fn mapped(&mut self) -> ToyIo<Self::Success, Self::Error> {
        let this = self.take().unwrap();
        match this.inner.eval().map(|x| (this.func)(x)) {
            Ok(eff) => eff,
            Err(e) => ToyIo::Fail(e),
        }
    }
}

pub fn unsafe_run_sync<R: Io>(r: R) -> Result<R::Success, R::Error> {
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
            .flat_map(|()| ToyIo::fail(StrError("some error")))
            .flat_map(|()| ToyIo::effect(|| println!("second effect - will not run")))
            .recover(|e| ToyIo::effect(move || println!("recovered from failure: {e}")));

        unsafe_run_sync(effect).expect("cannot execute");
    }
}
