use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use num::complex::Complex64;
use thiserror::Error;

#[derive(Debug, PartialEq, derive_more::Display)]
pub enum ArgType {
    Variable,
    Function,
    Function2,
}

#[derive(Debug, Error, PartialEq)]
pub enum MissingError {
    #[error("Missing entry for {} with name {}", .0, .1)]
    MissingEntry(ArgType, String),
    #[error("Missing value for {} with name {}", .0, .1)]
    MissingValue(ArgType, String),
}

#[derive(Default)]
pub struct Args {
    variables: HashMap<String, Option<Complex64>>,
    functions: HashMap<String, Option<Box<dyn Fn(Complex64) -> Complex64>>>,
    functions2: HashMap<String, Option<Box<dyn Fn(Complex64, Complex64) -> Complex64>>>,
}

pub struct ArgsErased(pub Args);

impl AsRef<Args> for ArgsErased {
    fn as_ref(&self) -> &Args {
        &self.0
    }
}

pub struct ArgsRefErased<'l>(pub &'l Args);

impl Debug for ArgsRefErased<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn display_map<V>(map: &HashMap<String, V>) -> String {
            format!(
                "{{{}}}",
                map.keys()
                    .into_iter()
                    .map(|s| s.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
        f.debug_struct("Args")
            .field("variables", &display_map(&self.0.variables))
            .field("functions", &display_map(&self.0.functions))
            .field("functions2", &display_map(&self.0.functions2))
            .finish()
    }
}

impl Debug for ArgsErased {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        ArgsRefErased(&self.0).fmt(f)
    }
}

impl PartialEq for ArgsRefErased<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.0.variables.keys().collect::<HashSet<_>>().eq(&other
            .0
            .variables
            .keys()
            .collect::<HashSet<_>>())
            && self.0.functions.keys().collect::<HashSet<_>>().eq(&other
                .0
                .functions
                .keys()
                .collect::<HashSet<_>>())
            && self.0.functions2.keys().collect::<HashSet<_>>().eq(&other
                .0
                .functions2
                .keys()
                .collect::<HashSet<_>>())
    }
}

impl PartialEq for ArgsErased {
    fn eq(&self, other: &Self) -> bool {
        ArgsRefErased(&self.0).eq(&ArgsRefErased(&other.0))
    }
}

impl Args {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn erased(self) -> ArgsErased {
        ArgsErased(self)
    }

    pub fn erased_ref(&self) -> ArgsRefErased<'_> {
        ArgsRefErased(self)
    }

    pub fn register_variable(&mut self, name: impl Into<String>) {
        self.variables.entry(name.into()).or_default();
    }

    pub fn register_function(&mut self, name: impl Into<String>) {
        self.functions.entry(name.into()).or_default();
    }

    pub fn register_function2(&mut self, name: impl Into<String>) {
        self.functions2.entry(name.into()).or_default();
    }

    pub fn unregister_variable(&mut self, name: impl AsRef<str>) {
        self.variables.remove_entry(name.as_ref());
    }

    pub fn unregister_function(&mut self, name: impl AsRef<str>) {
        self.functions.remove_entry(name.as_ref());
    }

    pub fn unregister_function2(&mut self, name: impl AsRef<str>) {
        self.functions2.remove_entry(name.as_ref());
    }

    pub fn assign_variable<N: AsRef<str> + Into<String>>(&mut self, name: N, value: Complex64) {
        if let Some(opt) = self.variables.get_mut(name.as_ref()) {
            *opt = Some(value);
        } else {
            self.variables.insert(name.into(), Some(value));
        }
    }

    pub fn assign_function<N: AsRef<str> + Into<String>>(
        &mut self,
        name: N,
        value: impl Fn(Complex64) -> Complex64 + 'static,
    ) {
        let value = Box::new(value);
        if let Some(opt) = self.functions.get_mut(name.as_ref()) {
            *opt = Some(value);
        } else {
            self.functions.insert(name.into(), Some(value));
        }
    }

    pub fn assign_function2<N: AsRef<str> + Into<String>>(
        &mut self,
        name: N,
        value: impl Fn(Complex64, Complex64) -> Complex64 + 'static,
    ) {
        let value = Box::new(value);
        if let Some(opt) = self.functions2.get_mut(name.as_ref()) {
            *opt = Some(value);
        } else {
            self.functions2.insert(name.into(), Some(value));
        }
    }

    pub fn get_variable<N: AsRef<str> + Into<String>>(
        &self,
        name: N,
    ) -> Result<&Complex64, MissingError> {
        self.variables
            .get(name.as_ref())
            .ok_or_else(|| MissingError::MissingEntry(ArgType::Variable, name.as_ref().to_owned()))
            .and_then(|opt| {
                if let Some(value) = opt {
                    Ok(value)
                } else {
                    Err(MissingError::MissingValue(ArgType::Variable, name.into()))
                }
            })
    }

    pub fn get_function<N: AsRef<str> + Into<String>>(
        &self,
        name: N,
    ) -> Result<&dyn Fn(Complex64) -> Complex64, MissingError> {
        self.functions
            .get(name.as_ref())
            .ok_or_else(|| MissingError::MissingEntry(ArgType::Function, name.as_ref().to_owned()))
            .and_then(|opt| {
                if let Some(value) = opt {
                    Ok(value.as_ref())
                } else {
                    Err(MissingError::MissingValue(ArgType::Variable, name.into()))
                }
            })
    }

    pub fn get_function2<N: AsRef<str> + Into<String>>(
        &self,
        name: N,
    ) -> Result<&dyn Fn(Complex64, Complex64) -> Complex64, MissingError> {
        self.functions2
            .get(name.as_ref())
            .ok_or_else(|| MissingError::MissingEntry(ArgType::Function2, name.as_ref().to_owned()))
            .and_then(|opt| {
                if let Some(value) = opt {
                    Ok(value.as_ref())
                } else {
                    Err(MissingError::MissingValue(ArgType::Variable, name.into()))
                }
            })
    }

    pub fn merge(&mut self, other: Self) {
        self.variables.extend(other.variables.into_iter());
        self.functions.extend(other.functions.into_iter());
        self.functions2.extend(other.functions2.into_iter());
    }

    pub fn variables(&self) -> impl IntoIterator<Item = &String> {
        self.variables.keys()
    }

    pub fn variables_mut(&mut self) -> impl IntoIterator<Item = (&String, &mut Option<Complex64>)> {
        self.variables.iter_mut()
    }

    pub fn functions(&self) -> impl IntoIterator<Item = &String> {
        self.functions.keys()
    }

    pub fn functions_mut(
        &mut self,
    ) -> impl IntoIterator<Item = (&String, &mut Option<Box<dyn Fn(Complex64) -> Complex64>>)> {
        self.functions.iter_mut()
    }

    pub fn functions2(&self) -> impl IntoIterator<Item = &String> {
        self.functions2.keys()
    }

    pub fn functions2_mut(
        &mut self,
    ) -> impl IntoIterator<
        Item = (
            &String,
            &mut Option<Box<dyn Fn(Complex64, Complex64) -> Complex64>>,
        ),
    > {
        self.functions2.iter_mut()
    }
}
