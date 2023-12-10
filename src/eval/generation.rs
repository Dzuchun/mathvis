use std::num::NonZeroUsize;

use nom::{
    branch::alt,
    combinator::{all_consuming, map, map_parser, opt},
    error::{Error as NError, ParseError},
    sequence::{separated_pair, tuple},
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
    #[error("No expected token found")]
    NoToken,
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
    node(tokens).map(|(_, n)| n).map_err(|err| match err {
        Err::Incomplete(needed) => GenerationError::Empty(needed),
        Err::Error(e) | Err::Failure(e) => e,
    })
}

fn node(tokens: &[Token]) -> Res {
    assert_parens(tokens)?;
    alt((map_parser(paren, raw_node), raw_node))(tokens)
}

fn raw_node(tokens: &[Token]) -> Res {
    alt((
        all_consuming(function_call),
        all_consuming(variable),
        all_consuming(constant),
        all_consuming(bin_operator),
    ))(tokens)
}

fn bin_operator(tokens: &[Token]) -> Res {
    let (_, (node1, op, node2)) = find_bin_operator(tokens)?;
    let (_, node1) = node(node1)?;
    let (_, node2) = node(node2)?;
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
    static PREDESCENCE: [Operator; 5] = [
        Operator::Plus,
        Operator::Minus,
        Operator::Star,
        Operator::Slash,
        Operator::Cap,
    ];
    for operator in &PREDESCENCE {
        let mut inner = 0;
        let inds = tokens
            .into_iter()
            .enumerate()
            .filter_map(|(i, token)| {
                match token {
                    Token::Open(_) => inner += 1,
                    Token::Close(_) => inner -= 1,
                    Token::Operator(op) if op == operator => {
                        if inner == 0 {
                            return Some(i);
                        }
                    }
                    _ => {}
                };
                None
            })
            .filter(|i| *i != 0)
            .collect::<Vec<_>>();
        if let Some(&ind) = inds.get(inds.len() / 2) {
            return Ok((&[], (&tokens[..ind], operator.clone(), &tokens[ind + 1..])));
        }
    }
    Err(Err::Error(GenerationError::NoToken))
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
        map(tuple((opt(sign), imaginary_part)), |(minus, im)| {
            Complex64::new(0.0, if let Some(true) = minus { -im } else { im })
        }),
        map(tuple((opt(sign), real_part)), |(minus, re)| {
            Complex64::new(if let Some(true) = minus { -re } else { re }, 0.0)
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
                    tuple((identifier, map_parser(paren, node))),
                    |(name, node)| NamedFunction::new(name, node).into(),
                ),
                map(
                    tuple((identifier, map_parser(paren, comma_separated))),
                    |(name, (node1, node2))| NamedFunction::new(name, (node1, node2).into()).into(),
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

fn imaginary_unit(tokens: &[Token]) -> Res<()> {
    let t = tokens
        .get(0)
        .ok_or(Err::Error(GenerationError::Empty(Needed::Size(
            NonZeroUsize::try_from(1).expect("1 is not zero"),
        ))))?;
    let Token::ImaginaryUnit = t else {
        return Err(Err::Error(GenerationError::BadToken(
            "imaginary unit",
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

fn comma_separated(tokens: &[Token]) -> Res<(Node<Complex64>, Node<Complex64>)> {
    let (_, (arg1, arg2)) = find_comma(tokens)?;
    let (_, arg1) = node(arg1)?;
    let (_, arg2) = node(arg2)?;
    Ok((&[], (arg1, arg2)))
}

fn find_comma(tokens: &[Token]) -> Res<(&[Token], &[Token])> {
    let mut inline = Vec::new();
    for (i, token) in tokens.into_iter().enumerate() {
        match token {
            Token::Open(t) => {
                inline.push(t);
            }
            Token::Close(t) => {
                if &t
                    != inline
                        .last()
                        .ok_or(Err::Failure(GenerationError::UnpairedParen))?
                {
                    return Err(Err::Failure(GenerationError::UnpairedParen));
                }

                inline.pop();
            }
            Token::Comma => {
                if inline.is_empty() {
                    return Ok((&[], (&tokens[..i], &tokens[i + 1..])));
                }
            }
            _ => {}
        }
    }
    Err(Err::Error(GenerationError::NoToken))
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

    let mut inner = 1;
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

#[cfg(test)]
mod tests {

    use std::collections::HashSet;

    use num::complex::Complex64;
    use rand::Rng;

    use crate::{
        aet::{
            arithmetic::{Addition, Exponentiation, Multiplication, Negation},
            functions::NamedFunction,
            Constant, Node, Variable,
        },
        eval::{Args, EvaluationError},
        parse::{Operator, Token},
    };

    use super::parse;

    #[derive(Debug)]
    enum ComparisionError {
        ArgList(HashSet<String>, HashSet<String>),
        Evaluation(EvaluationError),
        Evaluated(Args, Complex64, Complex64),
    }

    fn rand_complex() -> Complex64 {
        Complex64::new(rand::thread_rng().gen(), rand::thread_rng().gen())
    }

    // not a 100%-proof test, but whatever
    fn compare(node1: Node<Complex64>, node2: Node<Complex64>) -> Result<(), ComparisionError> {
        let args1 = node1.args();
        let args2 = node2.args();
        let mut arg_list1 = args1.0.keys().cloned().collect::<HashSet<String>>();
        let arg_list2 = args2.0.keys().cloned().collect::<HashSet<String>>();
        if arg_list1 != arg_list2 {
            return Err(ComparisionError::ArgList(arg_list1, arg_list2));
        }
        let mut args = args1;
        arg_list1.remove("sin");
        args.insert_unifunction("sin", Complex64::sin);
        fn sum_sq((x, y): (Complex64, Complex64)) -> Complex64 {
            x * x + y * y
        }
        // dbg!(&args);
        arg_list1.remove("sum_sq");
        args.insert_bifunction("sum_sq", sum_sq);
        for _ in 0..100 {
            for name in &arg_list1 {
                args.insert_value(name, rand_complex());
            }

            let res1 = node1
                .evaluate(&args)
                .map_err(ComparisionError::Evaluation)?;
            let res2 = node2
                .evaluate(&args)
                .map_err(ComparisionError::Evaluation)?;
            if res1 != res2 {
                return Err(ComparisionError::Evaluated(args, res1, res2));
            }
        }
        Ok(())
    }

    macro_rules! test {
        ($name:ident, tokens = [$($token:expr), *], expected = $expected:expr) => {
            #[test]
            fn $name() {
                // arrage

                // act
                let ast = parse(&[$($token), *]).expect("Should be able to parse");

                // assert
                compare(ast, $expected).expect("Should get identical results");
            }
        };
    }

    // following tests reinforce that complex constant recognition works as expected:
    test! {constant1,
    tokens = [Token::Number(1.0f64)],
    expected = Constant(Complex64::new(1.0, 0.0)).into()}
    test! {constant2,
    tokens = [Token::ImaginaryUnit],
    expected = Constant(Complex64::new(0.0, 1.0)).into()}
    test! {constant3,
    tokens = [
        Token::Number(1.0f64),
        Token::Operator(Operator::Star),
        Token::ImaginaryUnit
    ], expected = Constant(Complex64::new(0.0, 1.0)).into()}
    test! {constant4,
    tokens = [
        Token::Number(1.0f64),
        Token::Operator(Operator::Plus),
        Token::ImaginaryUnit
    ], expected = Constant(Complex64::new(1.0, 1.0)).into()}
    test! {constant5,
    tokens = [
        Token::Operator(Operator::Minus),
        Token::Number(1.0f64),
        Token::Operator(Operator::Plus),
        Token::Number(1.0),
        Token::Operator(Operator::Star),
        Token::ImaginaryUnit
    ], expected = Constant(Complex64::new(-1.0, 1.0)).into()}
    test! {constant6,
    tokens = [
        Token::Operator(Operator::Minus),
        Token::Number(1.0f64),
        Token::Operator(Operator::Plus),
        Token::Number(1.0), Token::ImaginaryUnit
    ], expected = Constant(Complex64::new(-1.0, 1.0)).into()}
    test! {constant7,
    tokens = [
        Token::Operator(Operator::Minus),
        Token::ImaginaryUnit,
        Token::Operator(Operator::Star),
        Token::Number(1.0f64),
        Token::Operator(Operator::Plus),
        Token::Number(1.0)
    ], expected = Constant(Complex64::new(1.0, -1.0)).into()}
    test! {constant8,
    tokens = [
        Token::Operator(Operator::Minus),
        Token::ImaginaryUnit,
        Token::Operator(Operator::Plus),
        Token::Number(1.0)
    ], expected = Constant(Complex64::new(1.0, -1.0)).into()}
    test! {constant9,
    tokens = [
        Token::Operator(Operator::Minus),
        Token::ImaginaryUnit
    ], expected = Constant(Complex64::new(0.0, -1.0)).into()}

    // following tests reinforce that variable recognition works as expected:
    test! {variable1,
    tokens = [Token::Ident("x".to_owned())],
    expected = Variable::new("x").into()}
    test! {variable2,
    tokens = [Token::Ident("a_123".to_owned())],
    expected = Variable::new("a_123").into()}

    // following tests reinforce that function calls work as expected:
    test! {function1,
    tokens = [
        Token::Ident("sin".to_owned()),
        Token::Open(crate::parse::ParenType::Paren),
        Token::Ident("x".to_owned()),
        Token::Close(crate::parse::ParenType::Paren)
    ], expected = NamedFunction::<Complex64, Complex64>::new("sin", Variable::<Complex64>::new("x").into()).into()}

    test! {function2,
    tokens = [
        Token::Ident("sum_sq".to_owned()),
        Token::Open(crate::parse::ParenType::Paren),
        Token::Ident("x".to_owned()),
        Token::Comma,
        Token::Ident("y".to_owned()),
        Token::Close(crate::parse::ParenType::Paren)
    ], expected = NamedFunction::<(Complex64, Complex64), Complex64>::new("sum_sq", Node::from((Variable::<Complex64>::new("x").into(), Variable::<Complex64>::new("y").into()))).into()}

    test! {function3,
    tokens = [
        Token::Operator(Operator::Minus),
        Token::Ident("sin".to_owned()),
        Token::Open(crate::parse::ParenType::Paren),
        Token::Ident("x".to_owned()),
        Token::Close(crate::parse::ParenType::Paren)
    ], expected = Negation::new(NamedFunction::<Complex64, Complex64>::new("sin", Variable::<Complex64>::new("x").into()).into()).into()}

    test! {bin_operator1, tokens = [
        Token::Ident("x".to_owned()),
        Token::Operator(Operator::Plus),
        Token::Ident("y".to_owned()),
        Token::Operator(Operator::Star),
        Token::Number(2f64.into())
    ], expected = Addition::new(Variable::<Complex64>::new("x").into(), Multiplication::new(Variable::<Complex64>::new("y").into(), Constant::<Complex64>(2f64.into()).into()).into()).into()}

    test! {bin_operator2, tokens = [
        Token::Open(crate::parse::ParenType::Bracket),
        Token::Operator(Operator::Minus),
        Token::Ident("x".to_owned()),
        Token::Operator(Operator::Plus),
        Token::Number(4f64.into()),
        Token::Close(crate::parse::ParenType::Bracket),
        Token::Operator(Operator::Star),
        Token::Ident("sin".to_owned()),
        Token::Open(crate::parse::ParenType::Brace),
        Token::Ident("y".to_owned()),
        Token::Close(crate::parse::ParenType::Brace),
        Token::Operator(Operator::Cap),
        Token::Number(2f64.into())
    ], expected = Multiplication::new(
        Addition::new(
            Negation::new(Variable::<Complex64>::new("x").into()).into(),
            Constant(Complex64::new(4.0, 0.0)).into()
        ).into(),
        Exponentiation::new(
            NamedFunction::new(
                "sin",
                Variable::<Complex64>::new("y").into()
            ).into(),
            Constant(Complex64::new(2.0, 0.0)).into()
        ).into()
    ).into()}
}
