use num::{complex::Complex64, Complex};

use crate::{
    lexer::Token,
    parser::{parse, GenerationError},
};

use self::args::{Args, MissingError};

pub mod args;
pub mod arithmetic;
pub mod functions;

pub trait Evaluatable {
    type Res;

    fn evaluate(&self, arguments: &Args) -> Result<Self::Res, MissingError>;

    fn args(&self) -> Args;
}

pub type Node<T = Complex64> = Box<dyn Evaluatable<Res = T>>;
pub type NodeRef<'l, T = Complex64> = &'l dyn Evaluatable<Res = T>;
type Res<T = Complex64> = Result<T, MissingError>;

impl<Ev: Evaluatable + 'static> From<Ev> for Node<Ev::Res> {
    fn from(value: Ev) -> Self {
        Box::new(value)
    }
}

pub struct EvaluationTree(Node);

impl EvaluationTree {
    pub fn from_tokens(tokens: &[Token]) -> Result<Self, GenerationError> {
        Ok(Self(parse(&tokens)?))
    }
}

impl Evaluatable for EvaluationTree {
    type Res = Complex64;

    fn evaluate(&self, arguments: &Args) -> Result<Self::Res, MissingError> {
        self.0.evaluate(arguments)
    }

    fn args(&self) -> Args {
        self.0.args()
    }
}

impl<Res1, Res2> Evaluatable for (Node<Res1>, Node<Res2>) {
    type Res = (Res1, Res2);

    #[inline(always)]
    fn evaluate(&self, arguments: &Args) -> Res<Self::Res> {
        Ok((self.0.evaluate(arguments)?, self.1.evaluate(arguments)?))
    }

    fn args(&self) -> Args {
        let mut args = self.0.args();
        args.merge(self.1.args());
        args
    }
}

impl<N1: Evaluatable, N2: Evaluatable> Evaluatable for (N1, N2) {
    type Res = (N1::Res, N2::Res);

    #[inline(always)]
    fn evaluate(&self, arguments: &Args) -> Res<Self::Res> {
        Ok((self.0.evaluate(arguments)?, self.1.evaluate(arguments)?))
    }

    fn args(&self) -> Args {
        let mut args = self.0.args();
        args.merge(self.1.args());
        args
    }
}

#[derive(PartialEq)]
pub struct Constant(pub Complex64);

impl Constant {
    pub fn tr(self: Self) -> Self {
        let Self(Complex::<f64> { re, im }) = self;
        Self(Complex64::new(im, re))
    }
}

impl From<Complex64> for Constant {
    fn from(value: Complex64) -> Self {
        Self(value)
    }
}

impl From<f64> for Constant {
    fn from(value: f64) -> Self {
        Complex64::new(value, 0.0).into()
    }
}

impl Evaluatable for Constant {
    type Res = Complex64;

    #[inline(always)]
    fn evaluate(&self, _: &Args) -> Res {
        Ok(self.0)
    }

    fn args(&self) -> Args {
        Args::new()
    }
}

#[derive(PartialEq)]
pub struct Variable(String);

impl Variable {
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }
}

impl Evaluatable for Variable {
    type Res = Complex64;

    #[inline(always)]
    fn evaluate(&self, arguments: &Args) -> Res {
        arguments.get_variable(&self.0).copied()
    }

    fn args(&self) -> Args {
        let mut args = Args::new();
        args.register_variable(&self.0);
        args
    }
}

pub trait Operator {
    type Input;
    fn operate(&self, input: Self::Input) -> Complex64;
    fn inner(&self) -> NodeRef<Self::Input>;
}

impl<In, Op> Evaluatable for Op
where
    Op: Operator<Input = In>,
{
    type Res = Complex64;

    #[inline(always)]
    fn evaluate(&self, arguments: &Args) -> Res {
        Ok(self.operate(self.inner().evaluate(arguments)?))
    }

    fn args(&self) -> Args {
        self.inner().args()
    }
}

#[cfg(test)]
mod tests {

    use num::complex::Complex64;

    use crate::evaluation_tree::args::{Args, MissingError};

    use super::{Constant, Evaluatable, Variable};

    #[test]
    fn consts() {
        // arrange
        let calc = Constant::from(4.0);

        // act
        let args = calc.args();

        // assert
        for _ in 0..10 {
            assert_eq!(
                calc.evaluate(&args)
                    .expect("Should be able to evaluate constant"),
                Complex64::new(4.0, 0.0),
                "Constant value is always the same"
            );
        }
    }

    #[test]
    fn variable_should_exist() {
        // arrange
        let calc = Variable::new("x");
        let args = Args::new();

        // act
        let res = calc.evaluate(&args);

        // assert
        assert_eq!(
            res,
            Err(MissingError::MissingEntry(
                crate::evaluation_tree::args::ArgType::Variable,
                "x".to_owned()
            ))
        );
    }

    #[test]
    fn should_succeed() {
        // arrange
        let calc = Variable::new("x");
        let args = {
            let mut args = Args::new();
            args.assign_variable("x", 1.0f64.into());
            args
        };

        // act
        let res = calc.evaluate(&args);

        // assert
        assert_eq!(res, Ok(1.0f64.into()));
    }
}
