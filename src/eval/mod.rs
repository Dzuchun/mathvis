use std::collections::HashMap;

use num::complex::Complex64;
use thiserror::Error;

use crate::{aet::Node, parse::Token};

use self::generation::{parse, GenerationError};

mod generation;

#[derive(Debug, Error, PartialEq)]
pub enum EvaluationError {
    #[error("Missing entry for variable {}", .0)]
    MissingEntry(String),
    #[error("Missing value for a variable {}", .0)]
    MissingValue(String),
    #[error("Wrong type for a variable {}: {} expecteв", .0, .1)]
    WrongType(String, &'static str),
}

// TODO this is a temporary solution, I guess...?
// for some reason, fn(Complex64) -> Complex64 casts to dyn std::any::Any just fine
// but it just won't cast back. regular numbers do that flawlessly.
#[derive(Debug)]
enum Param {
    Number(Complex64),
    UniFunction(fn(Complex64) -> Complex64),
    BiFunction(fn((Complex64, Complex64)) -> Complex64),
}

#[derive(Debug)]
pub struct Args(HashMap<String, Option<Param>>);

impl Args {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn register(&mut self, name: impl ToString) {
        self.0.entry(name.to_string()).or_default();
    }

    pub fn unregister(&mut self, name: impl AsRef<str>) {
        self.0.remove_entry(name.as_ref());
    }

    pub fn insert_value(&mut self, name: impl ToString, value: Complex64) {
        self.insert(name, Param::Number(value));
    }

    pub fn insert_unifunction(
        &mut self,
        name: impl ToString,
        function: fn(Complex64) -> Complex64,
    ) {
        self.insert(name, Param::UniFunction(function));
    }

    pub fn insert_bifunction(
        &mut self,
        name: impl ToString,
        function: fn((Complex64, Complex64)) -> Complex64,
    ) {
        self.insert(name, Param::BiFunction(function));
    }

    fn insert(&mut self, name: impl ToString, param: Param) {
        self.0.insert(name.to_string(), Some(param));
    }

    pub fn get_value(&self, name: impl AsRef<str>) -> Result<&Complex64, EvaluationError> {
        self.get(name.as_ref()).and_then(|para| {
            if let Param::Number(val) = para {
                Ok(val)
            } else {
                Err(EvaluationError::WrongType(
                    name.as_ref().to_owned(),
                    "value",
                ))
            }
        })
    }

    pub fn get_unifunction(
        &self,
        name: impl AsRef<str>,
    ) -> Result<&fn(Complex64) -> Complex64, EvaluationError> {
        self.get(name.as_ref()).and_then(|para| {
            if let Param::UniFunction(func) = para {
                Ok(func)
            } else {
                Err(EvaluationError::WrongType(
                    name.as_ref().to_owned(),
                    "unifunction",
                ))
            }
        })
    }

    pub fn get_bifunction(
        &self,
        name: impl AsRef<str>,
    ) -> Result<&fn((Complex64, Complex64)) -> Complex64, EvaluationError> {
        self.get(name.as_ref()).and_then(|para| {
            if let Param::BiFunction(val) = para {
                Ok(val)
            } else {
                Err(EvaluationError::WrongType(
                    name.as_ref().to_owned(),
                    "bifunction",
                ))
            }
        })
    }

    fn get(&self, name: impl AsRef<str>) -> Result<&Param, EvaluationError> {
        /* self.0
        .get(name.as_ref())
        .ok_or_else(|| EvaluationError::MissingEntry(name.as_ref().to_owned()))?
        .as_ref()
        .ok_or_else(|| EvaluationError::MissingValue(name.as_ref().to_owned()))?
        .as_ref()
        .downcast_ref::<T>()
        .ok_or_else(|| {
            EvaluationError::WrongType(name.as_ref().to_owned(), std::any::type_name::<T>())
        }) */
        self.0
            .get(name.as_ref())
            .ok_or_else(|| EvaluationError::MissingEntry(name.as_ref().to_owned()))?
            .as_ref()
            .ok_or_else(|| EvaluationError::MissingValue(name.as_ref().to_owned()))
    }

    pub fn merge(&mut self, other: Self) {
        other.0.into_keys().for_each(|indent| self.register(indent));
    }
}

pub struct EvaluationTree<Res>(Node<Res>);

impl EvaluationTree<Complex64> {
    pub fn from_tokens(tokens: &[Token]) -> Result<Self, GenerationError> {
        Ok(Self(parse(&tokens)?))
    }
}
