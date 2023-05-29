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

    /// Maps the output of this effect into another type.
    pub fn map<U: 'static>(self, f: impl FnOnce(T) -> U + 'static) -> ToyIo<U, E> {
        ToyIo::FlatMap(Box::new(Some(FlatMapImpl {
            inner: self,
            func: move |x| ToyIo::succeed(f(x)),
        })))
    }

    /// Recovers if this effect fails.
    ///
    /// The input fed to `f` is the error of this effect.
    pub fn recover(self, f: impl FnOnce(E) -> Self + 'static) -> Self {
        Self::Recover(Box::new(self), Box::new(f))
    }

    /// Executes the given effect accumulating on each item of the iterator.
    pub fn for_each<I, F>(it: I, f: F) -> ToyIoForEach<I, F> {
        ToyIoForEach { it, func: f }
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

#[must_use]
pub struct ToyIoForEach<I, F> {
    it: I,
    func: F,
}

impl<
        Item: 'static,
        I: Iterator<Item = Item>,
        T: 'static,
        E: 'static,
        F: FnOnce(Item) -> ToyIo<T, E> + Clone + 'static,
    > Io for ToyIoForEach<I, F>
{
    type Success = Vec<T>;
    type Error = E;

    fn eval(self) -> Result<Self::Success, Self::Error> {
        self.it
            .fold(ToyIo::effect(|| Vec::new()), |acc, curr| {
                let func = self.func.clone();
                acc.flat_map(move |x| {
                    (func)(curr).map(|y| {
                        let mut z = x;
                        z.push(y);
                        z
                    })
                })
            })
            .eval()
    }
}

pub fn unsafe_run_sync<R: Io>(r: R) -> Result<R::Success, R::Error> {
    r.eval()
}

#[cfg(test)]
mod test {
    use std::{
        convert::Infallible,
        error::Error,
        fmt::Display,
        process::{Command, Stdio},
    };

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

    #[ignore = "should be tested by for_each_abort"]
    #[test]
    fn for_each() {
        let effect = ToyIo::<i32, Infallible>::for_each(0..100000, |x| {
            ToyIo::<_, Infallible>::effect(move || {
                println!("{x}");
                x
            })
        });

        unsafe_run_sync(effect).expect("cannot execute");
    }

    #[test]
    fn for_each_abort() {
        let mut process = Command::new("cargo")
            .args(["test", "--", "--ignored", "for_each"])
            .stderr(Stdio::null())
            .spawn()
            .expect("cannot spawn cargo");
        assert_eq!(process.wait().expect("cannot wait cargo").code(), Some(101))
    }
}
