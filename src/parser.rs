//! This module defines parser and relevant structs.

use std::num::NonZeroUsize;

use nom::{
    Err, Needed,
    branch::alt,
    combinator::{all_consuming, map, map_parser, opt},
    error::{Error as NError, ParseError},
    sequence::tuple,
};
use num::complex::Complex64;
use thiserror::Error;

use crate::{
    evaluation_tree::{
        Constant, Node, Variable,
        arithmetic::{Addition, Division, Exponentiation, Multiplication, Negation, Subtraction},
        functions::NamedFunction,
    },
    lexer::{Operator, Token, TokenType},
};

/// Represents error occurred during tree generation.
#[derive(Debug, Error)]
pub enum GenerationError<'l> {
    /// Error emitted by `nom`
    #[error("{0:?}")]
    NomError(NError<&'l [Token]>),
    /// There are no more tokens, but some were expected
    #[error("No tokens")]
    Empty(Needed),
    /// Unexpected token type encountered
    #[error("Bad token: {} expected, but {:?} found", .0, .1)]
    BadToken(TokenType, &'l Token),
    /// There's an unpaired grouping
    #[error("Paren was left unclosed")]
    UnpairedGrouping,
    /// Expected token was not found
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
        // TODO actually implement this, lol!
        other
    }
}

type Res<'l, T = Node<Complex64>> = Result<(&'l [Token], T), Err<GenerationError<'l>>>;

/// parses tokens into a node.
pub fn parse(tokens: &[Token]) -> Result<Node<Complex64>, GenerationError> {
    all_consuming(node)(tokens)
        .map(|(_, n)| n)
        .map_err(|err| match err {
            Err::Incomplete(needed) => GenerationError::Empty(needed),
            Err::Error(e) | Err::Failure(e) => e,
        })
}

fn node(tokens: &[Token]) -> Res {
    assert_parens(tokens)?;
    alt((all_consuming(map_parser(paren, raw_node)), raw_node))(tokens)
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
            Operator::Plus => Addition::new(node1, node2).into(),
            Operator::Minus => Subtraction::new(node1, node2).into(),
            Operator::Star => Multiplication::new(node1, node2).into(),
            Operator::Slash => Division::new(node1, node2).into(),
            Operator::Cap => Exponentiation::new(node1, node2).into(),
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
            .iter()
            .enumerate()
            .filter_map(|(i, token)| {
                match token {
                    Token::GroupOpen(_) => inner += 1,
                    Token::GroupClose(_) => inner -= 1,
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
        if let Some(&ind) = match operator {
            Operator::Plus | Operator::Star | Operator::Cap => inds.first(),
            Operator::Minus | Operator::Slash => inds.last(),
        } {
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
        map(tuple((number, opt(star), imaginary_unit)), |(n, _, ())| n),
        map(tuple((imaginary_unit, star, number)), |((), (), n)| n),
        map(imaginary_unit, |()| 1.0f64),
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
                    |(name, (node1, node2))| NamedFunction::new(name, (node1, node2)).into(),
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
            Negation::new(Variable::new(name)).into()
        } else {
            Variable::new(name).into()
        }
    })(tokens)
}

// general tokens

fn assert_parens(tokens: &[Token]) -> Res<()> {
    let mut stack = Vec::new();
    for token in tokens {
        if let Token::GroupOpen(t) = token {
            stack.push(t);
        }

        if let Token::GroupClose(t) = token {
            let Some(&e) = stack.last() else {
                return Err(Err::Failure(GenerationError::UnpairedGrouping));
            };
            if e != t {
                return Err(Err::Failure(GenerationError::UnpairedGrouping));
            }
            stack.pop();
        }
    }
    if stack.is_empty() {
        Ok((&[], ()))
    } else {
        Err(Err::Failure(GenerationError::UnpairedGrouping))
    }
}

fn number(tokens: &[Token]) -> Res<f64> {
    let token = tokens
        .first()
        .ok_or(Err::Error(GenerationError::Empty(Needed::Size(
            NonZeroUsize::try_from(1).expect("1 is not zero"),
        ))))?;
    let Token::Number(val) = token else {
        return Err(Err::Error(GenerationError::BadToken(
            TokenType::Constant,
            token,
        )));
    };
    Ok((&tokens[1..], *val))
}

fn plus(tokens: &[Token]) -> Res<()> {
    let t = tokens
        .first()
        .ok_or(Err::Error(GenerationError::Empty(Needed::Size(
            NonZeroUsize::try_from(1).expect("1 is not zero"),
        ))))?;
    let Token::Operator(Operator::Plus) = t else {
        return Err(Err::Error(GenerationError::BadToken(
            TokenType::Operator,
            t,
        )));
    };
    Ok((&tokens[1..], ()))
}

fn minus(tokens: &[Token]) -> Res<()> {
    let t = tokens
        .first()
        .ok_or(Err::Error(GenerationError::Empty(Needed::Size(
            NonZeroUsize::try_from(1).expect("1 is not zero"),
        ))))?;
    let Token::Operator(Operator::Minus) = t else {
        return Err(Err::Error(GenerationError::BadToken(
            TokenType::Operator,
            t,
        )));
    };
    Ok((&tokens[1..], ()))
}

fn star(tokens: &[Token]) -> Res<()> {
    let t = tokens
        .first()
        .ok_or(Err::Error(GenerationError::Empty(Needed::Size(
            NonZeroUsize::try_from(1).expect("1 is not zero"),
        ))))?;
    let Token::Operator(Operator::Star) = t else {
        return Err(Err::Error(GenerationError::BadToken(
            TokenType::Operator,
            t,
        )));
    };
    Ok((&tokens[1..], ()))
}

fn imaginary_unit(tokens: &[Token]) -> Res<()> {
    let t = tokens
        .first()
        .ok_or(Err::Error(GenerationError::Empty(Needed::Size(
            NonZeroUsize::try_from(1).expect("1 is not zero"),
        ))))?;
    let Token::ImaginaryUnit = t else {
        return Err(Err::Error(GenerationError::BadToken(
            TokenType::Operator,
            t,
        )));
    };
    Ok((&tokens[1..], ()))
}

fn identifier(tokens: &[Token]) -> Res<String> {
    let t = tokens
        .first()
        .ok_or(Err::Error(GenerationError::Empty(Needed::Size(
            NonZeroUsize::try_from(1).expect("1 is not zero"),
        ))))?;
    let Token::Identifier(name) = t else {
        return Err(Err::Error(GenerationError::BadToken(
            TokenType::Operator,
            t,
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
    for (i, token) in tokens.iter().enumerate() {
        match token {
            Token::GroupOpen(t) => {
                inline.push(t);
            }
            Token::GroupClose(t) => {
                if &t
                    != inline
                        .last()
                        .ok_or(Err::Failure(GenerationError::UnpairedGrouping))?
                {
                    return Err(Err::Failure(GenerationError::UnpairedGrouping));
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
    alt((map(plus, |()| false), map(minus, |()| true)))(tokens)
}

fn paren(tokens: &[Token]) -> Res<&[Token]> {
    let t = tokens
        .first()
        .ok_or(Err::Error(GenerationError::Empty(Needed::Size(
            NonZeroUsize::try_from(1).expect("1 is not zero"),
        ))))?;
    let Token::GroupOpen(t) = t else {
        return Err(Err::Error(GenerationError::BadToken(
            TokenType::Grouping,
            t,
        )));
    };

    let mut inner = 1;
    for i in 1..tokens.len() {
        let token = &tokens[i];
        if let Token::GroupOpen(e) = token {
            if t == e {
                inner += 1;
            }
        }
        if let Token::GroupClose(e) = token {
            if t == e {
                inner -= 1;

                if inner == 0 {
                    return Ok((&tokens[i + 1..], &tokens[1..i]));
                }
            }
        }
    }

    Err(Err::Failure(GenerationError::UnpairedGrouping))
}

#[allow(clippy::result_large_err, dead_code, reason = "It's a testing code")]
#[cfg(test)]
mod tests {

    use num::complex::Complex64;
    use rand::Rng;

    use crate::{
        evaluation_tree::{
            Constant, Evaluatable, Node, Variable,
            args::{ArgsErased, MissingError},
            arithmetic::{Addition, Division, Exponentiation, Multiplication, Negation},
            functions::NamedFunction,
        },
        lexer::{GroupingType, Operator, Token},
    };

    use super::parse;

    #[derive(Debug)]
    enum ComparisionError {
        ArgList(ArgsErased, ArgsErased),
        Evaluation(MissingError),
        Evaluated(ArgsErased, Complex64, Complex64),
    }

    fn rand_complex() -> Complex64 {
        Complex64::new(rand::rng().random(), rand::rng().random())
    }

    // not a 100%-proof test, but whatever
    fn compare(
        node1: Node,
        node2: impl Evaluatable<Res = Complex64>,
    ) -> Result<(), ComparisionError> {
        let args1 = node1.args();
        let args2 = node2.args();
        if args1.erased_ref() != args2.erased_ref() {
            return Err(ComparisionError::ArgList(args1.erased(), args2.erased()));
        }
        let mut args = args1;
        args.assign_function("sin", Complex64::sin);
        args.assign_function2("sum_sq", |x, y| x * x + y * y);

        for _ in 0..100 {
            for (_, val) in args.variables_mut() {
                *val = Some(rand_complex());
            }

            let res1 = node1
                .evaluate(&args)
                .map_err(ComparisionError::Evaluation)?;
            let res2 = node2
                .evaluate(&args)
                .map_err(ComparisionError::Evaluation)?;
            if res1 != res2 {
                return Err(ComparisionError::Evaluated(args.erased(), res1, res2));
            }
        }
        Ok(())
    }

    macro_rules! test {
        ($name:ident, tokens = [$($token:expr_2021), *], expected = $expected:expr_2021) => {
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
    expected = Constant::from(1.0f64)}
    test! {constant2,
    tokens = [Token::ImaginaryUnit],
    expected = Constant::from(1.0f64).tr()}
    test! {constant3,
    tokens = [
        Token::Number(1.0f64),
        Token::Operator(Operator::Star),
        Token::ImaginaryUnit
    ], expected = Constant::from(1.0f64).tr()}
    test! {constant4,
    tokens = [
        Token::Number(1.0f64),
        Token::Operator(Operator::Plus),
        Token::ImaginaryUnit
    ], expected = Constant(Complex64::new(1.0, 1.0))}
    test! {constant5,
    tokens = [
        Token::Operator(Operator::Minus),
        Token::Number(1.0f64),
        Token::Operator(Operator::Plus),
        Token::Number(1.0),
        Token::Operator(Operator::Star),
        Token::ImaginaryUnit
    ], expected = Constant(Complex64::new(-1.0, 1.0))}
    test! {constant6,
    tokens = [
        Token::Operator(Operator::Minus),
        Token::Number(1.0f64),
        Token::Operator(Operator::Plus),
        Token::Number(1.0), Token::ImaginaryUnit
    ], expected = Constant(Complex64::new(-1.0, 1.0))}
    test! {constant7,
    tokens = [
        Token::Operator(Operator::Minus),
        Token::ImaginaryUnit,
        Token::Operator(Operator::Star),
        Token::Number(1.0f64),
        Token::Operator(Operator::Plus),
        Token::Number(1.0)
    ], expected = Constant(Complex64::new(1.0, -1.0))}
    test! {constant8,
    tokens = [
        Token::Operator(Operator::Minus),
        Token::ImaginaryUnit,
        Token::Operator(Operator::Plus),
        Token::Number(1.0)
    ], expected = Constant(Complex64::new(1.0, -1.0))}
    test! {constant9,
    tokens = [
        Token::Operator(Operator::Minus),
        Token::ImaginaryUnit
    ], expected = Constant(Complex64::new(0.0, -1.0))}

    // following tests reinforce that variable recognition works as expected:
    test! {variable1,
    tokens = [Token::Identifier("x".to_owned())],
    expected = Variable::new("x")}
    test! {variable2,
    tokens = [Token::Identifier("a_123".to_owned())],
    expected = Variable::new("a_123")}

    // following tests reinforce that function calls work as expected:
    test! {function1,
    tokens = [
        Token::Identifier("sin".to_owned()),
        Token::GroupOpen(GroupingType::Parentheses),
        Token::Identifier("x".to_owned()),
        Token::GroupClose(GroupingType::Parentheses)
    ], expected = NamedFunction::new("sin", Variable::new("x"))}

    test! {function2,
    tokens = [
        Token::Identifier("sum_sq".to_owned()),
        Token::GroupOpen(GroupingType::Parentheses),
        Token::Identifier("x".to_owned()),
        Token::Comma,
        Token::Identifier("y".to_owned()),
        Token::GroupClose(GroupingType::Parentheses)
    ], expected = NamedFunction::new("sum_sq", (Variable::new("x"), Variable::new("y")))}

    test! {function3,
    tokens = [
        Token::Operator(Operator::Minus),
        Token::Identifier("sin".to_owned()),
        Token::GroupOpen(GroupingType::Parentheses),
        Token::Identifier("x".to_owned()),
        Token::GroupClose(GroupingType::Parentheses)
    ], expected = Negation::new(NamedFunction::new("sin", Variable::new("x")))}

    // following tests reinforce correct binary operator parsing
    // x + y * 2
    test! {bin_operator1, tokens = [
        Token::Identifier("x".to_owned()),
        Token::Operator(Operator::Plus),
        Token::Identifier("y".to_owned()),
        Token::Operator(Operator::Star),
        Token::Number(2f64)
    ], expected = Addition::new(Variable::new("x"), Multiplication::new(Variable::new("y"), Constant(2f64.into())))}

    // [-x + 4] * sin{y}^2
    test! {bin_operator2, tokens = [
        Token::GroupOpen(GroupingType::Brackets),
        Token::Operator(Operator::Minus),
        Token::Identifier("x".to_owned()),
        Token::Operator(Operator::Plus),
        Token::Number(4f64),
        Token::GroupClose(GroupingType::Brackets),
        Token::Operator(Operator::Star),
        Token::Identifier("sin".to_owned()),
        Token::GroupOpen(GroupingType::Braces),
        Token::Identifier("y".to_owned()),
        Token::GroupClose(GroupingType::Braces),
        Token::Operator(Operator::Cap),
        Token::Number(2f64)
    ], expected = Multiplication::new(
        Addition::new(
            Negation::new(Variable::new("x")),
            Constant(Complex64::new(4.0, 0.0))
        ),
        Exponentiation::new(
            NamedFunction::new(
                "sin",
                Variable::new("y")
            ),
            Constant(Complex64::new(2.0, 0.0))
        )
    )}

    // z ^ [z] + sin(1.0 / e ^ {i * phi})
    test! {bin_operator3, tokens = [
        Token::Identifier("z".to_owned()),
        Token::Operator(Operator::Cap),
        Token::GroupOpen(GroupingType::Brackets),
        Token::Identifier("z".to_owned()),
        Token::GroupClose(GroupingType::Brackets),
        Token::Operator(Operator::Plus),
        Token::Identifier("sin".to_owned()),
        Token::GroupOpen(GroupingType::Parentheses),
        Token::Number(1.0),
        Token::Operator(Operator::Slash),
        Token::Identifier("e".to_owned()),
        Token::Operator(Operator::Cap),
        Token::GroupOpen(GroupingType::Braces),
        Token::ImaginaryUnit,
        Token::Operator(Operator::Star),
        Token::Identifier("phi".to_owned()),
        Token::GroupClose(GroupingType::Braces),
        Token::GroupClose(GroupingType::Parentheses)
    ], expected = Addition::new(
        Exponentiation::new(
            Variable::new("z"),
            Variable::new("z"),
        ),
        NamedFunction::new(
            "sin",
            Division::new(
                Constant(Complex64::new(1.0, 0.0)),
                Exponentiation::new(
                    Variable::new("e"),
                    Multiplication::new(
                        Constant(Complex64::new(0.0, 1.0)),
                        Variable::new("phi")
                    )
                )
            )
        )
    )
    }
}
