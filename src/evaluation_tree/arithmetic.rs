use num::{complex::Complex64, traits::Pow};

use super::{Node, NodeRef, Operator};

type Complex64x2 = (Complex64, Complex64);

pub struct Negation(Node);

impl Negation {
    pub fn new(inner: impl Into<Node>) -> Self {
        Self(inner.into())
    }
}

impl Operator for Negation {
    type Input = Complex64;

    fn operate(&self, input: Self::Input) -> Complex64 {
        -input
    }

    fn inner(&self) -> NodeRef<'_, Self::Input> {
        self.0.as_ref()
    }
}

#[cfg(test)]
mod negation_tests {

    use alloc::borrow::ToOwned;
    use num::complex::Complex64;

    use crate::evaluation_tree::{
        args::{ArgType, Args, MissingError},
        arithmetic::Negation,
        Constant, Evaluatable, Variable,
    };

    #[test]
    fn consts() {
        // arrange
        let calc = Negation::new(Constant::from(-4.0));

        // act
        let args = calc.args();

        // assert
        for _ in 0..10 {
            assert_eq!(
                calc.evaluate(&args)
                    .expect("Should be able to evaluate constant"),
                Complex64::from(4.0),
                "Constant value is always the same"
            );
        }
    }

    #[test]
    fn should_demand_param() {
        // arrange
        let calc = Negation::new(Variable::new("x"));
        let args = Args::new();

        // act
        let res = calc.evaluate(&args);

        // assert
        assert_eq!(
            res,
            Err(MissingError::MissingEntry(
                ArgType::Variable,
                "x".to_owned()
            ))
        );
    }

    #[test]
    fn should_succeed() {
        // arrange
        let calc = Negation::new(Variable::new("x"));
        let mut args = Args::new();
        args.assign_variable("x", 1f64.into());

        // act
        let res = calc.evaluate(&args);

        // assert
        assert_eq!(res, Ok(Complex64::new(-1.0, 0.0)));
    }
}

pub struct Addition(Node<Complex64x2>);

impl Addition {
    pub fn new(in1: impl Into<Node>, in2: impl Into<Node>) -> Self {
        Self((in1.into(), in2.into()).into())
    }
}

impl Operator for Addition {
    type Input = Complex64x2;

    fn operate(&self, input: Self::Input) -> Complex64 {
        input.0 + input.1
    }

    fn inner(&self) -> NodeRef<Self::Input> {
        self.0.as_ref()
    }
}

#[cfg(test)]
mod addition_tests {

    use alloc::borrow::ToOwned;
    use num::complex::Complex64;

    use crate::evaluation_tree::{
        args::{ArgType, Args, MissingError},
        Evaluatable, Variable,
    };

    use super::Addition;

    #[test]
    fn should_demand_both() {
        // arrange
        let calc = Addition::new(Variable::new("x"), Variable::new("y"));
        let mut args = Args::new();

        // act + assert
        args.assign_variable("x", 1f64.into());
        assert_eq!(
            calc.evaluate(&args),
            Err(MissingError::MissingEntry(
                ArgType::Variable,
                "y".to_owned()
            ))
        );

        args.unregister_variable("x");
        args.assign_variable("y", 2f64.into());
        assert_eq!(
            calc.evaluate(&args),
            Err(MissingError::MissingEntry(
                ArgType::Variable,
                "x".to_owned()
            ))
        );

        args.assign_variable("x", 1f64.into());
        assert_eq!(calc.evaluate(&args), Ok(Complex64::new(3.0, 0.0)));
    }

    #[test]
    fn should_demand_one() {
        // arrange
        let calc = Addition::new(Variable::new("x"), Variable::new("x"));
        let mut args = Args::new();

        // act + assert
        args.assign_variable("y", 2f64.into());
        assert_eq!(
            calc.evaluate(&args),
            Err(MissingError::MissingEntry(
                ArgType::Variable,
                "x".to_owned()
            ))
        );

        args.register_variable("x");
        assert_eq!(
            calc.evaluate(&args),
            Err(MissingError::MissingValue(
                ArgType::Variable,
                "x".to_owned()
            ))
        );

        args.assign_variable("x", 1f64.into());
        assert_eq!(calc.evaluate(&args), Ok(Complex64::new(2.0, 0.0)));
    }
}

pub struct Subtraction(Node<Complex64x2>);

impl Subtraction {
    pub fn new(in1: impl Into<Node>, in2: impl Into<Node>) -> Self {
        Self((in1.into(), in2.into()).into())
    }
}

impl Operator for Subtraction {
    type Input = Complex64x2;

    fn operate(&self, input: Self::Input) -> Complex64 {
        input.0 - input.1
    }

    fn inner(&self) -> NodeRef<Self::Input> {
        self.0.as_ref()
    }
}

pub struct Multiplication(Node<Complex64x2>);

impl Multiplication {
    pub fn new(in1: impl Into<Node>, in2: impl Into<Node>) -> Self {
        Self((in1.into(), in2.into()).into())
    }
}

impl Operator for Multiplication {
    type Input = Complex64x2;

    fn operate(&self, input: Self::Input) -> Complex64 {
        input.0 * input.1
    }

    fn inner(&self) -> NodeRef<Self::Input> {
        self.0.as_ref()
    }
}

pub struct Division(Node<Complex64x2>);

impl Division {
    pub fn new(in1: impl Into<Node>, in2: impl Into<Node>) -> Self {
        Self((in1.into(), in2.into()).into())
    }
}

impl Operator for Division {
    type Input = Complex64x2;

    fn operate(&self, input: Self::Input) -> Complex64 {
        input.0 / input.1
    }

    fn inner(&self) -> NodeRef<Self::Input> {
        self.0.as_ref()
    }
}

pub struct Exponentiation(Node<Complex64x2>);

impl Exponentiation {
    pub fn new(in1: impl Into<Node>, in2: impl Into<Node>) -> Self {
        Self((in1.into(), in2.into()).into())
    }
}

impl Operator for Exponentiation {
    type Input = Complex64x2;

    fn operate(&self, input: Self::Input) -> Complex64 {
        input.0.pow(input.1)
    }

    fn inner(&self) -> NodeRef<Self::Input> {
        self.0.as_ref()
    }
}
