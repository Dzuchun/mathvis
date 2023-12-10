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
    #[error("Wrong type for a variable {}: {} expected", .0, .1)]
    WrongType(String, &'static str),
}

pub struct Args(HashMap<String, Option<Box<dyn std::any::Any>>>);

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

    pub fn insert(&mut self, name: impl ToString, value: impl std::any::Any) {
        self.0.insert(
            name.to_string(),
            Some(Box::new(value) as Box<dyn std::any::Any>),
        );
    }

    pub fn get<T: 'static>(&self, name: impl AsRef<str>) -> Result<&T, EvaluationError> {
        self.0
            .get(name.as_ref())
            .ok_or_else(|| EvaluationError::MissingEntry(name.as_ref().to_owned()))?
            .as_ref()
            .ok_or_else(|| EvaluationError::MissingValue(name.as_ref().to_owned()))?
            .as_ref()
            .downcast_ref::<T>()
            .ok_or_else(|| {
                EvaluationError::WrongType(name.as_ref().to_owned(), std::any::type_name::<T>())
            })
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
