use core::fmt::Display;
use std::ops::Not;

use crate::display_opt;

#[inline]
fn valid_sign(ch: char) -> bool {
    matches!(ch, '+' | '-')
}

#[inline]
fn valid_exponent_marker(ch: char) -> bool {
    matches!(ch, 'e' | 'E')
}

#[inline]
fn valid_decimal_point(ch: char) -> bool {
    matches!(ch, '.')
}

#[derive(
    Debug,
    Clone,
    Copy,
    Hash,
    PartialEq,
    Eq,
    derive_more::AsRef,
    derive_more::Deref,
    derive_more::Display,
)]
#[display("{_0}")]
pub struct Digits<D>(D);

impl<'s> Digits<&'s str> {
    #[tracing::instrument(level = "trace", ret, skip(input), fields(digits_input = input))]
    fn parse(input: &'s str) -> Option<(Self, &'s str)> {
        let end_pos = input
            .char_indices()
            .find_map(|(pos, ch)| ch.is_ascii_digit().not().then_some(pos))
            .unwrap_or(input.len());

        if end_pos == 0 {
            None
        } else {
            Some((Self(&input[..end_pos]), &input[end_pos..]))
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{e}{}{digits}", if let Some(s) = sign.as_ref() { s as &dyn Display } else { &"" as &dyn Display })]
pub struct Exponent<C, D> {
    e: C,
    sign: Option<C>,
    digits: Digits<D>,
}

impl<'s> Exponent<&'s str, &'s str> {
    #[tracing::instrument(level = "trace", ret, skip(input), fields(exponent_input = input))]
    fn parse(input: &'s str) -> Option<(Self, &'s str)> {
        let mut char_inds = input.char_indices();
        // if there's no first char, there's no exponent
        let (_, e_marker) = char_inds.next()?;
        if !valid_exponent_marker(e_marker) {
            // does not start with an exponent marker, bail
            return None;
        }

        // exponent marker cannot go by itself
        let (mut pos, maybe_sign) = char_inds.next()?;
        let e = &input[..pos];
        // figure out sign
        let sign;
        if valid_sign(maybe_sign) {
            // second char is a valid sign, query for next one
            let (sign_end, _) = char_inds.next()?;
            sign = Some(&input[pos..sign_end]);
            pos = sign_end;
        } else {
            sign = None;
        }
        let (digits, rest) = Digits::parse(&input[pos..])?;
        Some((Self { e, sign, digits }, rest))
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
pub enum Number<C, D> {
    #[display("int({int}e{})", display_opt(exp))]
    Integer {
        int: Digits<D>,
        exp: Option<Exponent<C, D>>,
    },
    #[display("float({int}{point}{frac}{})", display_opt(exp))]
    Decimal {
        int: Digits<D>,
        point: C,
        frac: Digits<D>,
        exp: Option<Exponent<C, D>>,
    },
    #[display("float({int}{point}{})", display_opt(exp))]
    DecimalInt {
        int: Digits<D>,
        point: C,
        // no frac part
        exp: Option<Exponent<C, D>>,
    },
    #[display("float({point}{})", display_opt(exp))]
    DecimalFraction {
        // no int part
        point: C,
        frac: Digits<D>,
        exp: Option<Exponent<C, D>>,
    },
}

impl<'s> Number<&'s str, &'s str> {
    #[tracing::instrument(level = "trace", skip(input), fields(number_input = input), ret)]
    pub fn parse(mut input: &'s str) -> Option<(Self, &'s str)> {
        let int_part: Option<Digits<&str>>;

        let frac_part: Option<Digits<&str>>;
        let exp: Option<Exponent<&str, &str>>;

        if let Some((digits, rest)) = Digits::parse(input) {
            int_part = Some(digits);
            input = rest;
            trace!(?int_part, input);
        } else {
            int_part = None;
        }

        let point = 'point: {
            // try parsing decimal point. if it's only a (optional) sign and integer part - it's a normal integer
            let mut char_inds = input.char_indices();
            let Some((_, maybe_point)) = char_inds.next() else {
                break 'point None;
            };
            trace!(%maybe_point);

            // try parsing the sign
            if !valid_decimal_point(maybe_point) {
                break 'point None;
            }

            // this is a valid point char, find there it ends
            let point;
            if let Some((point_end, _)) = char_inds.next() {
                point = &input[..point_end];
                input = &input[point_end..];
            } else {
                point = input;
                input = "";
            };
            Some(point)
        };
        trace!(?point, input);

        // try parsing fractional part
        if let Some((digits, rest)) = Digits::parse(input) {
            frac_part = Some(digits);
            input = rest;
            trace!(?frac_part, input);
        } else {
            frac_part = None;
        }

        // try parsing exponent
        if let Some((exponent, rest)) = Exponent::parse(input) {
            exp = Some(exponent);
            input = rest;
            trace!(?exp, input);
        } else {
            exp = None;
        }

        trace!(?int_part, ?point, ?frac_part);
        match (int_part, point, frac_part) {
            (Some(int), None, None) => Some((Self::Integer { int, exp }, input)),
            (Some(int), Some(point), Some(frac)) => Some((
                Self::Decimal {
                    int,
                    point,
                    frac,
                    exp,
                },
                input,
            )),
            (None, Some(point), Some(frac)) => {
                Some((Self::DecimalFraction { point, frac, exp }, input))
            }
            (Some(int), Some(point), None) => Some((Self::DecimalInt { int, point, exp }, input)),
            _ => None,
        }
    }
}

#[macro_export]
macro_rules! number {
    (? $input:literal) => {
        'number: {
            let Some((number, rest)) = $crate::lexer_v2::number::Number::parse(stringify!($input))
            else {
                break 'number None;
            };
            if !rest.trim().is_empty() {
                break 'number None;
            }
            Some(number)
        }
    };
    ($input:literal) => {
        number!(?$input).expect("Invalid number")
    };
}

#[cfg(test)]
mod tests;

#[cfg(test)]
mod prop_tests;
