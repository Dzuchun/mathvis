use std::num::NonZeroUsize;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char as pchar, one_of},
    combinator::cut,
    combinator::{all_consuming, map, map_parser, opt, recognize},
    error::{Error as NError, ParseError},
    multi::{fold_many1, many0, many0_count},
    number::complete::double,
    sequence::{pair, preceded, tuple},
    Err, Needed,
};
use num::complex::Complex64;
use thiserror::Error;

use crate::{
    aet::{self, arithmetic::Negation, functions::NamedFunction, Constant, Node, Variable},
    parse::{Operator, Token},
};

#[derive(Debug, Error)]
pub enum GenerationError<'l> {
    #[error("{0:?}")]
    NomError(NError<&'l [Token]>),
    #[error("No tokens")]
    Empty(Needed),
    #[error("Bad token: {} expected, but {} found", .0, .1)]
    BadToken(&'static str, &'static str),
    #[error("Paren was left unclosed")]
    UnpairedParen,
}

impl<'l> ParseError<&'l [Token]> for GenerationError<'l> {
    fn from_error_kind(input: &'l [Token], kind: nom::error::ErrorKind) -> Self {
        Self::NomError(NError::new(input, kind))
    }

    fn append(_input: &[Token], _kind: nom::error::ErrorKind, other: Self) -> Self {
        // not really sure what this is about,
        // but it seems that `nom` itself implements this function in a similar fashion,
        // so I guess it's not much of a harm.
        other
    }
}

type Res<'l, T = Node<Complex64>> = Result<(&'l [Token], T), Err<GenerationError<'l>>>;

pub fn parse(tokens: &[Token]) -> Result<Node<Complex64>, GenerationError> {
    all_consuming(raw_node)(tokens)
        .map(|(_, n)| n)
        .map_err(|err| match err {
            Err::Incomplete(needed) => GenerationError::Empty(needed),
            Err::Error(e) | Err::Failure(e) => e,
        })
}

fn node(tokens: &[Token]) -> Res {
    assert_parens(tokens)?;
    all_consuming(alt((map_parser(paren, raw_node), raw_node)))(tokens)
}

fn raw_node(tokens: &[Token]) -> Res {
    alt((bin_operator, function_call, variable, constant))(tokens)
}

fn bin_operator(tokens: &[Token]) -> Res {
    let (_, (node1, op, node2)) = find_bin_operator(tokens)?;
    let (_, node1) = cut(node)(node1)?;
    let (_, node2) = cut(node)(node2)?;
    Ok((
        &[],
        match op {
            Operator::Plus => aet::arithmetic::Addition::new(node1, node2).into(),
            Operator::Minus => aet::arithmetic::Subtraction::new(node1, node2).into(),
            Operator::Star => aet::arithmetic::Multiplication::new(node1, node2).into(),
            Operator::Slash => aet::arithmetic::Division::new(node1, node2).into(),
            Operator::Cap => aet::arithmetic::Exponentiation::new(node1, node2).into(),
        },
    ))
}

fn find_bin_operator(tokens: &[Token]) -> Res<(&[Token], Operator, &[Token])> {
    todo!()
}

fn constant(tokens: &[Token]) -> Res {
    let (rest, val) = alt((
        map(
            tuple((opt(sign), real_part, sign, imaginary_part)),
            |(re_minus, re, im_minus, im)| {
                Complex64::new(
                    if let Some(true) = re_minus { -re } else { re },
                    if im_minus { -im } else { im },
                )
            },
        ),
        map(
            tuple((opt(sign), imaginary_part, sign, real_part)),
            |(im_minus, im, re_minus, re)| {
                Complex64::new(
                    if re_minus { -re } else { re },
                    if let Some(true) = im_minus { -im } else { im },
                )
            },
        ),
        map(tuple((opt(sign), real_part)), |(minus, re)| {
            Complex64::new(if let Some(true) = minus { -re } else { re }, 0.0)
        }),
        map(tuple((opt(sign), imaginary_part)), |(minus, im)| {
            Complex64::new(0.0, if let Some(true) = minus { -im } else { im })
        }),
    ))(tokens)?;
    Ok((rest, Constant(val).into()))
}

fn real_part(tokens: &[Token]) -> Res<f64> {
    number(tokens)
}

fn imaginary_part(tokens: &[Token]) -> Res<f64> {
    alt((
        map(tuple((number, opt(star), imaginary_unit)), |(n, _, _)| n),
        map(tuple((imaginary_unit, star, number)), |(_, _, n)| n),
        map(imaginary_unit, |_| 1.0f64),
    ))(tokens)
}

fn function_call(tokens: &[Token]) -> Res {
    map(
        tuple((
            opt(sign),
            alt((
                map(
                    tuple((identifier, map_parser(paren, raw_node))),
                    |(name, node)| NamedFunction::new(name, node).into(),
                ),
                map(
                    tuple((
                        identifier,
                        map_parser(paren, tuple((raw_node, comma, raw_node))),
                    )),
                    |(name, (node1, _, node2))| {
                        NamedFunction::new(name, (node1, node2).into()).into()
                    },
                ),
            )),
        )),
        |(minus, call)| {
            if let Some(true) = minus {
                Negation::new(call).into()
            } else {
                call
            }
        },
    )(tokens)
}

fn variable(tokens: &[Token]) -> Res {
    map(tuple((opt(sign), identifier)), |(minus, name)| {
        if let Some(true) = minus {
            Negation::new(Variable::<Complex64>::new(name).into()).into()
        } else {
            Variable::new(name).into()
        }
    })(tokens)
}

// general tokens

fn assert_parens(tokens: &[Token]) -> Res<()> {
    let mut stack = Vec::new();
    for token in tokens {
        if let Token::Open(t) = token {
            stack.push(t);
        }

        if let Token::Close(t) = token {
            let Some(&e) = stack.last() else {
                return Err(Err::Failure(GenerationError::UnpairedParen));
            };
            if e != t {
                return Err(Err::Failure(GenerationError::UnpairedParen));
            }
            stack.pop();
        }
    }
    if !stack.is_empty() {
        Err(Err::Failure(GenerationError::UnpairedParen))
    } else {
        Ok((&[], ()))
    }
}

fn number(tokens: &[Token]) -> Res<f64> {
    let t = tokens
        .get(0)
        .ok_or(Err::Error(GenerationError::Empty(Needed::Size(
            NonZeroUsize::try_from(1).expect("1 is not zero"),
        ))))?;
    let Token::Number(val) = t else {
        return Err(Err::Error(GenerationError::BadToken(
            "number",
            t.description(),
        )));
    };
    Ok((&tokens[1..], *val))
}

fn plus(tokens: &[Token]) -> Res<()> {
    let t = tokens
        .get(0)
        .ok_or(Err::Error(GenerationError::Empty(Needed::Size(
            NonZeroUsize::try_from(1).expect("1 is not zero"),
        ))))?;
    let Token::Operator(Operator::Plus) = t else {
        return Err(Err::Error(GenerationError::BadToken(
            "plus operator",
            t.description(),
        )));
    };
    Ok((&tokens[1..], ()))
}

fn minus(tokens: &[Token]) -> Res<()> {
    let t = tokens
        .get(0)
        .ok_or(Err::Error(GenerationError::Empty(Needed::Size(
            NonZeroUsize::try_from(1).expect("1 is not zero"),
        ))))?;
    let Token::Operator(Operator::Minus) = t else {
        return Err(Err::Error(GenerationError::BadToken(
            "minus operator",
            t.description(),
        )));
    };
    Ok((&tokens[1..], ()))
}

fn star(tokens: &[Token]) -> Res<()> {
    let t = tokens
        .get(0)
        .ok_or(Err::Error(GenerationError::Empty(Needed::Size(
            NonZeroUsize::try_from(1).expect("1 is not zero"),
        ))))?;
    let Token::Operator(Operator::Star) = t else {
        return Err(Err::Error(GenerationError::BadToken(
            "start operator",
            t.description(),
        )));
    };
    Ok((&tokens[1..], ()))
}

fn slash(tokens: &[Token]) -> Res<()> {
    let t = tokens
        .get(0)
        .ok_or(Err::Error(GenerationError::Empty(Needed::Size(
            NonZeroUsize::try_from(1).expect("1 is not zero"),
        ))))?;
    let Token::Operator(Operator::Slash) = t else {
        return Err(Err::Error(GenerationError::BadToken(
            "slash operator",
            t.description(),
        )));
    };
    Ok((&tokens[1..], ()))
}

fn cap(tokens: &[Token]) -> Res<()> {
    let t = tokens
        .get(0)
        .ok_or(Err::Error(GenerationError::Empty(Needed::Size(
            NonZeroUsize::try_from(1).expect("1 is not zero"),
        ))))?;
    let Token::Operator(Operator::Cap) = t else {
        return Err(Err::Error(GenerationError::BadToken(
            "cap operator",
            t.description(),
        )));
    };
    Ok((&tokens[1..], ()))
}

fn imaginary_unit(tokens: &[Token]) -> Res<()> {
    let t = tokens
        .get(0)
        .ok_or(Err::Error(GenerationError::Empty(Needed::Size(
            NonZeroUsize::try_from(1).expect("1 is not zero"),
        ))))?;
    let Token::Operator(Operator::Plus) = t else {
        return Err(Err::Error(GenerationError::BadToken(
            "number",
            t.description(),
        )));
    };
    Ok((&tokens[1..], ()))
}

fn identifier(tokens: &[Token]) -> Res<String> {
    let t = tokens
        .get(0)
        .ok_or(Err::Error(GenerationError::Empty(Needed::Size(
            NonZeroUsize::try_from(1).expect("1 is not zero"),
        ))))?;
    let Token::Ident(name) = t else {
        return Err(Err::Error(GenerationError::BadToken(
            "identifier",
            t.description(),
        )));
    };
    Ok((&tokens[1..], name.to_string()))
}

fn comma(tokens: &[Token]) -> Res<()> {
    let t = tokens
        .get(0)
        .ok_or(Err::Error(GenerationError::Empty(Needed::Size(
            NonZeroUsize::try_from(1).expect("1 is not zero"),
        ))))?;
    let Token::Comma = t else {
        return Err(Err::Error(GenerationError::BadToken(
            "comma",
            t.description(),
        )));
    };
    Ok((&tokens[1..], ()))
}

fn sign(tokens: &[Token]) -> Res<bool> {
    alt((map(plus, |_| false), map(minus, |_| true)))(tokens)
}

fn paren(tokens: &[Token]) -> Res<&[Token]> {
    let t = tokens
        .get(0)
        .ok_or(Err::Error(GenerationError::Empty(Needed::Size(
            NonZeroUsize::try_from(1).expect("1 is not zero"),
        ))))?;
    let Token::Open(t) = t else {
        return Err(Err::Error(GenerationError::BadToken(
            "opening paren",
            t.description(),
        )));
    };

    let mut inner = 0;
    for i in 1..tokens.len() {
        let token = &tokens[i];
        if let Token::Open(e) = token {
            if t == e {
                inner += 1;
            }
        }
        if let Token::Close(e) = token {
            if t == e {
                inner -= 1;

                if inner == 0 {
                    return Ok((&tokens[i + 1..], &tokens[1..i]));
                }
            }
        }
    }

    Err(Err::Failure(GenerationError::UnpairedParen))
}
