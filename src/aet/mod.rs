use std::marker::PhantomData;

use num::complex::Complex64;

use crate::eval::{Args, EvaluationError};

pub mod arithmetic;
pub mod functions;

pub trait Calculable {
    type Res;
    fn evaluate(&self, arguments: &Args) -> Result<Self::Res, EvaluationError>;

    fn args(&self) -> Args;
}

impl<C: Calculable + 'static> From<C> for Node<C::Res> {
    fn from(value: C) -> Self {
        Box::new(value)
    }
}

pub type Node<Res> = Box<dyn Calculable<Res = Res>>;
pub type NodeRef<'l, Res> = &'l dyn Calculable<Res = Res>;

impl<Res1, Res2> Calculable for (Node<Res1>, Node<Res2>) {
    type Res = (Res1, Res2);

    #[inline(always)]
    fn evaluate(&self, arguments: &Args) -> Result<Self::Res, EvaluationError> {
        Ok((self.0.evaluate(arguments)?, self.1.evaluate(arguments)?))
    }

    fn args(&self) -> Args {
        let mut args = self.0.args();
        args.merge(self.1.args());
        args
    }
}

#[derive(PartialEq)]
pub struct Constant<T>(pub T);

impl<Res: Clone> Calculable for Constant<Res> {
    type Res = Res;
    #[inline(always)]
    fn evaluate(&self, _: &Args) -> Result<Self::Res, EvaluationError> {
        Ok(self.0.clone())
    }

    fn args(&self) -> Args {
        Args::new()
    }
}

#[derive(PartialEq)]
pub struct Variable<T>(String, PhantomData<T>);

impl<T> Variable<T> {
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into(), PhantomData)
    }
}

/* 
impl<Res: Clone + 'static> Calculable for Variable<Res> {
    type Res = Res;

    #[inline(always)]
    fn evaluate(&self, arguments: &Args) -> Result<Self::Res, EvaluationError> {
        arguments.get(self.0.as_str()).cloned()
    }

    fn args(&self) -> Args {
        let mut args = Args::new();
        args.register(self.0.clone());
        args
    }
}
 */

impl Calculable for Variable<Complex64> {
    type Res = Complex64;

    #[inline(always)]
    fn evaluate(&self, arguments: &Args) -> Result<Self::Res, EvaluationError> {
        arguments.get_value(self.0.as_str()).cloned()
    }

    fn args(&self) -> Args {
        let mut args = Args::new();
        args.register(self.0.clone());
        args
    }
}


pub trait Operator {
    type In;
    type Out;
    fn operate(&self, input: Self::In) -> Self::Out;
    fn inner(&self) -> NodeRef<Self::In>;
}

impl<In, Out, Op> Calculable for Op
where
    Op: Operator<In = In, Out = Out>,
{
    type Res = Op::Out;

    #[inline(always)]
    fn evaluate(&self, arguments: &Args) -> Result<Self::Res, EvaluationError> {
        Ok(self.operate(self.inner().evaluate(arguments)?))
    }

    fn args(&self) -> Args {
        self.inner().args()
    }
}

#[cfg(test)]
mod tests {

    use num::complex::Complex64;

    use crate::eval::{Args, EvaluationError};

    use super::{Calculable, Constant, Variable};

    #[test]
    fn consts() {
        // arrange
        let calc = Constant(4);

        // act
        let args = calc.args();

        // assert
        for _ in 0..10 {
            assert_eq!(
                calc.evaluate(&args)
                    .expect("Should be able to evaluate constant"),
                4,
                "Constant value is always the same"
            );
        }
    }

    #[test]
    fn variable_should_exist() {
        // arrange
        let calc = Variable::<Complex64>::new("x");
        let args = Args::new();

        // act
        let res = calc.evaluate(&args);

        // assert
        assert_eq!(res, Err(EvaluationError::MissingEntry("x".to_owned())));
    }

    #[test]
    fn variables_should_have_same_type() {
        // arrange
        let calc = Variable::<Complex64>::new("x");
        let args = {
            let mut args = Args::new();
            args.insert_unifunction("x", Complex64::sin);
            args
        };

        // act
        let res = calc.evaluate(&args);

        // assert
        assert_eq!(res, Err(EvaluationError::WrongType("x".to_owned(), "value")));
    }
    

    #[test]
    fn should_succeed() {
        // arrange
        let calc = Variable::<Complex64>::new("x");
        let args = {
            let mut args = Args::new();
            args.insert_value("x", 1.0f64.into());
            args
        };

        // act
        let res = calc.evaluate(&args);

        // assert
        assert_eq!(res, Ok(1.0f64.into()));
    }
}
