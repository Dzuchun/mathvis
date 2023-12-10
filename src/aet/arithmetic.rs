use num::traits::Pow;
use std::ops::{Add, Div, Mul, Neg, Sub};

use super::{Node, NodeRef, Operator};

pub struct Negation<In>(Node<In>);

impl<In> Negation<In> {
    pub fn new(inner: Node<In>) -> Self {
        Self(inner)
    }
}

impl<In, Out> Operator for Negation<In>
where
    In: Neg<Output = Out>,
{
    type In = In;
    type Out = Out;

    #[inline(always)]
    fn operate(&self, input: In) -> Self::Out {
        -input
    }

    fn inner(&self) -> NodeRef<'_, Self::In> {
        self.0.as_ref()
    }
}

#[cfg(test)]
mod negation_tests {

    use crate::{
        aet::{arithmetic::Negation, Args, Calculable, Constant, Variable},
        eval::EvaluationError,
    };

    #[test]
    fn consts() {
        // arrange
        let calc = Negation::new(Constant(4).into());

        // act
        let args = calc.args();

        // assert
        for _ in 0..10 {
            assert_eq!(
                calc.evaluate(&args)
                    .expect("Should be able to evaluate constant"),
                -4,
                "Constant value is always the same"
            );
        }
    }

    #[test]
    fn should_demand_param() {
        // arrange
        let calc = Negation::new(Variable::<i32>::new("x").into());
        let args = Args::new();

        // act
        let res = calc.evaluate(&args);

        // assert
        assert_eq!(res, Err(EvaluationError::MissingEntry("x".to_owned())));
    }

    #[test]
    fn should_have_same_type() {
        // arrange
        let calc = Negation::new(Variable::<i32>::new("x").into());
        let mut args = Args::new();
        args.insert("x", 1u8);

        // act
        let res = calc.evaluate(&args);

        // assert
        assert_eq!(res, Err(EvaluationError::WrongType("x".to_owned(), "i32")));
    }

    #[test]
    fn should_succeed() {
        // arrange
        let calc = Negation::new(Variable::<i32>::new("x").into());
        let mut args = Args::new();
        args.insert("x", 1i32);

        // act
        let res = calc.evaluate(&args);

        // assert
        assert_eq!(res, Ok(-1i32));
    }
}

pub struct Addition<In1, In2>(Node<(In1, In2)>);

impl<In1: 'static, In2: 'static> Addition<In1, In2> {
    pub fn new(in1: Node<In1>, in2: Node<In2>) -> Self {
        Self((in1, in2).into())
    }
}

impl<In1, In2, Out> Operator for Addition<In1, In2>
where
    In1: Add<In2, Output = Out>,
{
    type In = (In1, In2);
    type Out = Out;

    #[inline(always)]
    fn operate(&self, input: Self::In) -> Self::Out {
        input.0 + input.1
    }

    fn inner(&self) -> NodeRef<Self::In> {
        self.0.as_ref()
    }
}

#[cfg(test)]
mod addition_tests {

    use crate::{
        aet::{Args, Calculable, Variable},
        eval::EvaluationError,
    };

    use super::Addition;

    #[test]
    fn should_demand_both() {
        // arrange
        let calc = Addition::new(
            Variable::<i32>::new("x").into(),
            Variable::<i32>::new("y").into(),
        );
        let mut args = Args::new();

        // act + assert
        args.insert("x", 1i32);
        assert_eq!(
            calc.evaluate(&args),
            Err(EvaluationError::MissingEntry("y".to_owned()))
        );

        args.unregister("x");
        args.insert("y", 2i32);
        assert_eq!(
            calc.evaluate(&args),
            Err(EvaluationError::MissingEntry("x".to_owned()))
        );

        args.insert("x", 1i32);
        assert_eq!(calc.evaluate(&args), Ok(3i32));
    }

    #[test]
    fn should_demand_one() {
        // arrange
        let calc = Addition::new(
            Variable::<i32>::new("x").into(),
            Variable::<i32>::new("x").into(),
        );
        let mut args = Args::new();

        // act + assert
        args.insert("y", 2i32);
        assert_eq!(
            calc.evaluate(&args),
            Err(EvaluationError::MissingEntry("x".to_owned()))
        );

        args.register("x");
        assert_eq!(
            calc.evaluate(&args),
            Err(EvaluationError::MissingValue("x".to_owned()))
        );

        args.insert("x", 1u8);
        assert_eq!(
            calc.evaluate(&args),
            Err(EvaluationError::WrongType("x".to_owned(), "i32"))
        );

        args.insert("x", 1i32);
        assert_eq!(calc.evaluate(&args), Ok(2i32));
    }
}

pub struct Subtraction<In1, In2>(Node<(In1, In2)>);

impl<In1: 'static, In2: 'static> Subtraction<In1, In2> {
    pub fn new(in1: Node<In1>, in2: Node<In2>) -> Self {
        Self((in1, in2).into())
    }
}

impl<In1, In2, Out> Operator for Subtraction<In1, In2>
where
    In1: Sub<In2, Output = Out>,
{
    type In = (In1, In2);
    type Out = Out;

    #[inline(always)]
    fn operate(&self, input: Self::In) -> Self::Out {
        input.0 - input.1
    }

    fn inner(&self) -> NodeRef<Self::In> {
        self.0.as_ref()
    }
}

pub struct Multiplication<In1, In2>(Node<(In1, In2)>);

impl<In1: 'static, In2: 'static> Multiplication<In1, In2> {
    pub fn new(in1: Node<In1>, in2: Node<In2>) -> Self {
        Self((in1, in2).into())
    }
}

impl<In1, In2, Out> Operator for Multiplication<In1, In2>
where
    In1: Mul<In2, Output = Out>,
{
    type In = (In1, In2);
    type Out = Out;

    #[inline(always)]
    fn operate(&self, input: Self::In) -> Self::Out {
        input.0 * input.1
    }

    fn inner(&self) -> NodeRef<Self::In> {
        self.0.as_ref()
    }
}

pub struct Division<In1, In2>(Node<(In1, In2)>);

impl<In1: 'static, In2: 'static> Division<In1, In2> {
    pub fn new(in1: Node<In1>, in2: Node<In2>) -> Self {
        Self((in1, in2).into())
    }
}

impl<In1, In2, Out> Operator for Division<In1, In2>
where
    In1: Div<In2, Output = Out>,
{
    type In = (In1, In2);
    type Out = Out;

    #[inline(always)]
    fn operate(&self, input: Self::In) -> Self::Out {
        input.0 / input.1
    }

    fn inner(&self) -> NodeRef<Self::In> {
        self.0.as_ref()
    }
}

pub struct Exponentiation<In1, In2>(Node<(In1, In2)>);

impl<In1: 'static, In2: 'static> Exponentiation<In1, In2> {
    pub fn new(in1: Node<In1>, in2: Node<In2>) -> Self {
        Self((in1, in2).into())
    }
}

impl<In1, In2, Out> Operator for Exponentiation<In1, In2>
where
    In1: Pow<In2, Output = Out>,
{
    type In = (In1, In2);
    type Out = Out;

    #[inline(always)]
    fn operate(&self, input: Self::In) -> Self::Out {
        input.0.pow(input.1)
    }

    fn inner(&self) -> NodeRef<Self::In> {
        self.0.as_ref()
    }
}
