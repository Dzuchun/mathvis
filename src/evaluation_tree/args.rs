//! This module defines `Args` object and all of the operations that can be performed with it.
//!
//! `Args` represents the way identifiers should be resolved during evaluation. Currently, there are three valid uses for identifiers (corresponding with variants of [`ArgType`]):
//! - variable
//! - function (single argument)
//! - function2 (two arguments)
//!
//! To actually evaluate the tree, runtime must know values that variables are actually equal to and exact operations represented by functions.
//!
//! Variable/function can be in tree states regarding to some `Args` object:
//! - *unregistered*: `Args` doesn't know it. In case it's required at evaluation, [`MissingError::MissingEntry`] error will be returned.
//! - *registered*: `Args` knows it exists, but doesn't have a definition for it yet. Used by trees to communicate variables they need. In case it's required at evaluation, [`MissingError::MissingValue`] error will be returned.
//! - *assigned*: `Args` knows about it and has a definition. This is the only state of variable/function that won't cause an error if asked for at evaluation.
//!
//! Variables/functions can be assigned to multiple times, this would overwrite them each time.
//!
//! Variables/functions can be unregistered too. Although, there's not much use for it apart from debugging.

use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use num::complex::Complex64;
use thiserror::Error;

/// Represents types of arguments.
///
/// - [`ArgType::Variable`] stands for regular complex variable.s
/// - [`ArgType::Function`] stands for single-complex-argument complex-valued function.
/// - [`ArgType::Function2`] stands for two-complex-argument complex-value function.
#[derive(Debug, PartialEq, derive_more::Display)]
pub enum ArgType {
    Variable,
    Function,
    Function2,
}

/// Represents error that had happened during evaluation.
///
/// - [`MissingError::MissingEntry`] represents that [`Args`] doesn't know about an argument it's asked about.
/// - [`MissingError::MissingValue`] represents that [`Args`] knows about an argument, but it was not assigned yet.
#[derive(Debug, Error, PartialEq)]
pub enum MissingError {
    #[error("Missing entry for {} with name {}", .0, .1)]
    MissingEntry(ArgType, String),
    #[error("Missing value for {} with name {}", .0, .1)]
    MissingValue(ArgType, String),
}

type Function = dyn Fn(Complex64) -> Complex64;
type Function2 = dyn Fn(Complex64, Complex64) -> Complex64;

/// Represents argument list as well as their values during evaluation with [`crate::evaluation_tree::Evaluatable::evaluate`] function. This object is required to perform it.
#[derive(Default)]
#[allow(clippy::module_name_repetitions)]
pub struct Args {
    variables: HashMap<String, Option<Complex64>>,
    functions: HashMap<String, Option<Box<Function>>>,
    functions2: HashMap<String, Option<Box<Function2>>>,
}

/// An erased form of [`Args`] that does not `Display` or `Eq` by actual argument values/states.
#[allow(clippy::module_name_repetitions)]
pub struct ArgsErased(pub Args);

impl AsRef<Args> for ArgsErased {
    fn as_ref(&self) -> &Args {
        &self.0
    }
}

/// An erased reference to [`Args`] that does not `Display` or `Eq` by actual argument values/states.
#[allow(clippy::module_name_repetitions)]
pub struct ArgsRefErased<'l>(pub &'l Args);

impl Debug for ArgsRefErased<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn display_map<V>(map: &HashMap<String, V>) -> String {
            format!(
                "{{{}}}",
                map.keys()
                    .map(String::as_str)
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
    /// Creates empty [`Args`] object.
    pub fn new() -> Self {
        Self::default()
    }

    /// Converts object into erased form.
    pub fn erased(self) -> ArgsErased {
        ArgsErased(self)
    }

    /// Gets erased reference to an object.
    pub fn erased_ref(&self) -> ArgsRefErased<'_> {
        ArgsRefErased(self)
    }

    /// Registers (but not assigns!) variable with a certain name.
    pub fn register_variable(&mut self, name: impl Into<String>) {
        self.variables.entry(name.into()).or_default();
    }

    /// Registers (but not assigns!) function with a certain name.
    pub fn register_function(&mut self, name: impl Into<String>) {
        self.functions.entry(name.into()).or_default();
    }

    /// Registers (but not assigns!) function2 with a certain name.
    pub fn register_function2(&mut self, name: impl Into<String>) {
        self.functions2.entry(name.into()).or_default();
    }

    /// Unregisters variable with a certain name, if present.
    pub fn unregister_variable(&mut self, name: impl AsRef<str>) {
        self.variables.remove_entry(name.as_ref());
    }

    /// Unregisters function with a certain name, if present.
    pub fn unregister_function(&mut self, name: impl AsRef<str>) {
        self.functions.remove_entry(name.as_ref());
    }

    /// Unregisters function2 with a certain name, if present.
    pub fn unregister_function2(&mut self, name: impl AsRef<str>) {
        self.functions2.remove_entry(name.as_ref());
    }

    /// Assigns to a variable with certain name (and registers it, if necessary)
    pub fn assign_variable<N: AsRef<str> + Into<String>>(&mut self, name: N, value: Complex64) {
        if let Some(opt) = self.variables.get_mut(name.as_ref()) {
            *opt = Some(value);
        } else {
            self.variables.insert(name.into(), Some(value));
        }
    }

    /// Assigns to a function with certain name (and registers it, if necessary)
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

    /// Assigns to a function2 with certain name (and registers it, if necessary)
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

    /// Retrieves reference to a value of variable. May fail with [`MissingError`], if variable is not registered or not assigned to.
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

    /// Retrieves reference to a function. May fail with [`MissingError`], if function was not defined.
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

    /// Retrieves reference to a function2. May fail with [`MissingError`], if function2 was not defined.
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

    /// **! THIS OPERATION IS NOT SYMMETRIC**
    ///
    /// Merges this [`Args`] object with another. Should be noted, that only registered variable sets merge, but states do not - this always prefers state of a variable in `this` instance.
    pub fn merge(&mut self, other: Self) {
        self.variables.extend(other.variables);
        self.functions.extend(other.functions);
        self.functions2.extend(other.functions2);
    }

    /// Gets iterator over registered variable names.
    pub fn variables(&self) -> impl IntoIterator<Item = &String> {
        self.variables.keys()
    }

    /// Gets iterator over registered variable names as well as their values - [`Option::None`] represents no value, while [`Option::Some`] represents assigned variable.
    pub fn variables_mut(&mut self) -> impl IntoIterator<Item = (&String, &mut Option<Complex64>)> {
        self.variables.iter_mut()
    }

    /// Gets iterator over registered function names.
    pub fn functions(&self) -> impl IntoIterator<Item = &String> {
        self.functions.keys()
    }

    /// Gets iterator over registered function names as well as their definitions - [`Option::None`] represents no definition, while [`Option::Some`] represents defined function.
    pub fn functions_mut(
        &mut self,
    ) -> impl IntoIterator<Item = (&String, &mut Option<Box<dyn Fn(Complex64) -> Complex64>>)> {
        self.functions.iter_mut()
    }

    /// Gets iterator over registered function2 names.
    pub fn functions2(&self) -> impl IntoIterator<Item = &String> {
        self.functions2.keys()
    }

    /// Gets iterator over registered function2 names as well as their definitions - [`Option::None`] represents no definition, while [`Option::Some`] represents defined function2.
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
