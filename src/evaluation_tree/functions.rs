//! This module defines function-related objects that can be used as [`Node`].
//!
//! These include:
//! - [`Functional`]: fully-contained function definition along with inner node, requires no additions arguments
//! - [`NamedFunction`]: functions that has a name and has to be defined by [`Args`] for evaluation
//!
//! In presented API, only [`NamedFunction`]s are used for now. Although, I might add lots of default functions that will automatically resolve to their [`Functional`] counterparts during parsing.

use num::complex::Complex64;

use super::{
    Evaluatable, Node, NodeRef, Operator,
    args::{Args, MissingError},
};

/// A defined function node, requires no additional definition.
pub struct Functional<Func, In>(Func, Node<In>);

impl<Func, In> Operator for Functional<Func, In>
where
    Func: Fn(In) -> Complex64,
{
    type Input = In;

    fn operate(&self, input: Self::Input) -> Complex64 {
        self.0(input)
    }

    fn inner(&self) -> NodeRef<Self::Input> {
        self.1.as_ref()
    }
}

impl<Func, In> Functional<Func, In> {
    pub fn new(func: Func, inner: impl Into<Node<In>>) -> Self {
        Self(func, inner.into())
    }
}

/// Named function node, requires definition through [`Args`]
pub struct NamedFunction<In>(String, Node<In>);

impl<In> NamedFunction<In> {
    pub fn new(name: impl Into<String>, inner: impl Into<Node<In>>) -> Self {
        NamedFunction(name.into(), inner.into())
    }
}

impl Evaluatable for NamedFunction<Complex64> {
    type Res = Complex64;

    fn evaluate(&self, arguments: &Args) -> Result<Self::Res, MissingError> {
        let function = arguments.get_function(&self.0)?;
        let inner = self.1.evaluate(arguments)?;
        Ok(function(inner))
    }

    fn args(&self) -> Args {
        let mut args = self.1.args();
        args.register_function(&self.0);
        args
    }
}

impl Evaluatable for NamedFunction<(Complex64, Complex64)> {
    type Res = Complex64;

    fn evaluate(&self, arguments: &Args) -> Result<Self::Res, MissingError> {
        let function = arguments.get_function2(self.0.as_str())?;
        let (arg1, arg2) = self.1.evaluate(arguments)?;
        Ok(function(arg1, arg2))
    }

    fn args(&self) -> Args {
        let mut args = self.1.args();
        args.register_function2(&self.0);
        args
    }
}

#[cfg(test)]
mod tests {
    use num::complex::Complex64;

    use crate::evaluation_tree::{
        Args, Constant, Evaluatable, Variable,
        args::{ArgType, MissingError},
    };

    use super::Functional;

    #[test]
    fn should_evaluate_const() {
        // arrange
        let calc = Functional::new(Complex64::acos, Constant::from(0.5f64));
        let args = Args::new();

        // act
        let res = calc.evaluate(&args);

        // assert
        assert_eq!(res, Ok(Complex64::from(0.5f64).acos()));
    }

    #[test]
    fn should_evaluate_variable() {
        // arrange
        let calc = Functional::new(Complex64::acos, Variable::new("x"));
        let mut args = Args::new();

        // act + assert
        assert_eq!(
            calc.evaluate(&args),
            Err(MissingError::MissingEntry(ArgType::Variable, "x".into()))
        );
        args.assign_variable("x", 0.4f64.into());
        assert_eq!(calc.evaluate(&args), Ok(Complex64::new(0.4, 0.0).acos()));
    }
}
