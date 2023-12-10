use std::marker::PhantomData;

use super::{Calculable, Node, NodeRef, Operator};

pub struct Functional<Func, In>(Func, Node<In>);

impl<Func, In, Out> Operator for Functional<Func, In>
where
    Func: Fn(In) -> Out,
{
    type In = In;
    type Out = Out;

    fn operate(&self, input: Self::In) -> Self::Out {
        self.0(input)
    }

    fn inner(&self) -> NodeRef<Self::In> {
        self.1.as_ref()
    }
}

impl<Func, In> Functional<Func, In> {
    pub fn new(func: Func, inner: Node<In>) -> Self {
        Self(func, inner)
    }
}

pub struct NamedFunction<In, Out>(String, Node<In>, PhantomData<Out>);

impl<In, Out> NamedFunction<In, Out> {
    pub fn new(name: String, inner: Node<In>) -> Self {
        NamedFunction(name, inner, PhantomData)
    }
}

impl<In: 'static, Out: 'static> Calculable for NamedFunction<In, Out> {
    type Res = Out;

    fn evaluate(
        &self,
        arguments: &crate::eval::Args,
    ) -> Result<Self::Res, crate::eval::EvaluationError> {
        let function = arguments.get::<Box<dyn Fn(In) -> Out>>(self.0.as_str())?;
        let inner = self.1.evaluate(arguments)?;
        Ok(function(inner))
    }

    fn args(&self) -> crate::eval::Args {
        let mut args = self.1.args();
        args.register(self.0.clone());
        args
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        aet::{Args, Calculable, Constant, Variable},
        eval::EvaluationError,
    };

    use super::Functional;

    #[test]
    fn should_evaluate_const() {
        // arrange
        let calc = Functional::new(f64::acos, Constant(0.5f64).into());
        let args = Args::new();

        // act
        let res = calc.evaluate(&args);

        // assert
        assert_eq!(res, Ok(0.5f64.acos()));
    }

    #[test]
    fn should_evaluate_variable() {
        // arrange
        let calc = Functional::new(f64::acos, Variable::new("x").into());
        let mut args = Args::new();

        // act + assert
        assert_eq!(
            calc.evaluate(&args),
            Err(EvaluationError::MissingEntry("x".to_owned()))
        );
        args.insert("x", 0.4f64);
        assert_eq!(calc.evaluate(&args), Ok(0.4f64.acos()));
    }
}
