//! This module defines lexer and structs relevant to it.

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char as pchar, one_of},
    combinator::{map, recognize},
    error::Error,
    multi::{fold_many1, many0, many0_count},
    number::complete::double,
    sequence::{pair, preceded},
};

use core::str::FromStr;

use crate::{
    display_opt,
    lexer_v2::{
        number::Number,
        token::{SpecialKind, Token},
    },
};

type Res<'l, T = Token> = nom::IResult<&'l str, T, Error<&'l str>>;

/// Represents type of grouping.
#[derive(Debug, PartialEq, Clone)]
pub enum GroupingType {
    /// ()
    Parentheses,
    /// []
    Brackets,
    /// {}
    Braces,
}

/// Represents operators (not necessary corresponding to actual [`crate::evaluation_tree::Operator`]s).
#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    Cap,
}

/// Represents a single token.
#[derive(Debug, PartialEq)]
pub enum Token {
    GroupOpen(GroupingType),
    GroupClose(GroupingType),
    Operator(Operator),
    Comma,
    ImaginaryUnit,
    Number(f64),
    Identifier(String),
}

<<<<<<< HEAD
=======
#[derive(Debug, Clone, Copy, thiserror::Error)]
#[error("Unsupported token: {_0}")]
pub struct UnsupportedToken<'s>(Token<&'s str, &'s str, &'s str>);

impl<'s> TryFrom<Token<&'s str, &'s str, &'s str>> for OldToken {
    type Error = UnsupportedToken<'s>;

    fn try_from(value: Token<&'s str, &'s str, &'s str>) -> Result<Self, Self::Error> {
        let token = match value {
            Token::Grouping {
                kind: crate::lexer_v2::token::GroupingKind::Parentheses,
                side: crate::lexer_v2::token::GroupingSide::Left,
            } => Self::GroupOpen(GroupingType::Parentheses),
            Token::Grouping {
                kind: crate::lexer_v2::token::GroupingKind::Brackets,
                side: crate::lexer_v2::token::GroupingSide::Left,
            } => Self::GroupOpen(GroupingType::Brackets),
            Token::Grouping {
                kind: crate::lexer_v2::token::GroupingKind::Braces,
                side: crate::lexer_v2::token::GroupingSide::Left,
            } => Self::GroupOpen(GroupingType::Braces),
            Token::Grouping {
                kind: crate::lexer_v2::token::GroupingKind::Parentheses,
                side: crate::lexer_v2::token::GroupingSide::Right,
            } => Self::GroupClose(GroupingType::Parentheses),
            Token::Grouping {
                kind: crate::lexer_v2::token::GroupingKind::Brackets,
                side: crate::lexer_v2::token::GroupingSide::Right,
            } => Self::GroupClose(GroupingType::Brackets),
            Token::Grouping {
                kind: crate::lexer_v2::token::GroupingKind::Braces,
                side: crate::lexer_v2::token::GroupingSide::Right,
            } => Self::GroupClose(GroupingType::Braces),
            Token::Special(SpecialKind::Comma) => Self::Comma,
            Token::Special(SpecialKind::Plus) => Self::Operator(Operator::Plus),
            Token::Special(SpecialKind::Minus) => Self::Operator(Operator::Minus),
            Token::Special(SpecialKind::Star) => Self::Operator(Operator::Star),
            Token::Special(SpecialKind::Slash) => Self::Operator(Operator::Slash),
            Token::Special(SpecialKind::Cap) => Self::Operator(Operator::Cap),
            Token::Number(Number::Integer { int, exp }) => Self::Number(
                f64::from_str(&format!("{int}.0{}", display_opt(&exp)))
                    .map_err(|_| UnsupportedToken(value))?,
            ),
            Token::Number(Number::Decimal {
                int,
                point,
                frac,
                exp,
            }) => Self::Number(
                f64::from_str(&format!("{int}{point}{frac}{}", display_opt(&exp)))
                    .map_err(|_| UnsupportedToken(value))?,
            ),
            Token::Number(Number::DecimalInt { int, point, exp }) => Self::Number(
                f64::from_str(&format!("{int}{point}0{}", display_opt(&exp)))
                    .map_err(|_| UnsupportedToken(value))?,
            ),
            Token::Number(Number::DecimalFraction { point, frac, exp }) => Self::Number(
                f64::from_str(&format!("0{point}{frac}{}", display_opt(&exp)))
                    .map_err(|_| UnsupportedToken(value))?,
            ),
            Token::Word(word) if *word == "i" => Self::ImaginaryUnit,
            Token::Word(word) => Self::Identifier(word.to_string()),
            _ => return Err(UnsupportedToken(value)),
        };

        Ok(token)
    }
}

>>>>>>> 7d0e9b0 (impl app)
/// Represents token type (used for debug)
#[derive(Debug, PartialEq, derive_more::Display)]
pub enum TokenType {
    Grouping,
    Operator,
    Comma,
    Constant,
    Identifier,
}

fn paren_open(i: &str) -> Res {
    map(one_of("([{"), |c| {
        Token::GroupOpen(match c {
            '(' => GroupingType::Parentheses,
            '[' => GroupingType::Brackets,
            '{' => GroupingType::Braces,
            _ => unreachable!(),
        })
    })(i)
}

fn paren_close(i: &str) -> Res {
    map(one_of(")]}"), |c| {
        Token::GroupClose(match c {
            ')' => GroupingType::Parentheses,
            ']' => GroupingType::Brackets,
            '}' => GroupingType::Braces,
            _ => unreachable!(),
        })
    })(i)
}

fn operator(i: &str) -> Res {
    map(one_of("+-*/^"), |c| {
        Token::Operator(match c {
            '+' => Operator::Plus,
            '-' => Operator::Minus,
            '*' => Operator::Star,
            '/' => Operator::Slash,
            '^' => Operator::Cap,
            _ => unreachable!(),
        })
    })(i)
}

fn comma(i: &str) -> Res {
    map(pchar(','), |_| Token::Comma)(i)
}

fn imaginary_unit(i: &str) -> Res {
    map(pchar('i'), |_| Token::ImaginaryUnit)(i)
}

// source: https://github.com/rust-bakery/nom/blob/main/doc/nom_recipes.md#identifiers
fn identifier(i: &str) -> Res {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
        |name: &str| Token::Identifier(name.to_owned()),
    )(i)
}

fn number(i: &str) -> Res {
    map(double, Token::Number)(i)
}

fn whitespace(i: &str) -> Res<()> {
    Ok((many0(one_of(" \t\x0c\n"))(i)?.0, ()))
}

/// lexes the input
pub fn lex(i: &str) -> Res<Vec<Token>> {
    fold_many1(
        preceded(
            whitespace,
            alt((
                operator,
                number,
                paren_open,
                paren_close,
                comma,
                imaginary_unit,
                identifier,
            )),
        ),
        Vec::new,
        |mut acc, token| {
            acc.push(token);
            acc
        },
    )(i)
}

#[cfg(test)]
mod tests {
<<<<<<< HEAD
=======
    use alloc::borrow::ToOwned;
    use alloc::vec::Vec;

>>>>>>> 7d0e9b0 (impl app)
    use crate::lexer::GroupingType;
    use crate::lexer_v2::token::Token;
    use crate::lexer_v2::StrTokenParser;

    use super::lex;
    use super::Token;

    #[test]
    fn tokens() {
        // arrange
        let input = "2 + x^2 - (3x^2 - 7y / sin[z])";

        // act
        let tokens = lex(input);

        // assert
        assert_eq!(
            tokens,
            Ok((
                "",
                vec![
                    Token::Number(2f64),
                    Token::Operator(crate::lexer::Operator::Plus),
                    Token::Identifier("x".to_owned()),
                    Token::Operator(crate::lexer::Operator::Cap),
                    Token::Number(2f64),
                    Token::Operator(crate::lexer::Operator::Minus),
                    Token::GroupOpen(GroupingType::Parentheses),
                    Token::Number(3f64),
                    Token::Identifier("x".to_owned()),
                    Token::Operator(crate::lexer::Operator::Cap),
                    Token::Number(2f64),
                    Token::Operator(crate::lexer::Operator::Minus),
                    Token::Number(7f64),
                    Token::Identifier("y".to_owned()),
                    Token::Operator(crate::lexer::Operator::Slash),
                    Token::Identifier("sin".to_owned()),
                    Token::GroupOpen(GroupingType::Brackets),
                    Token::Identifier("z".to_owned()),
                    Token::GroupClose(GroupingType::Brackets),
                    Token::GroupClose(GroupingType::Parentheses),
                ]
            ))
        );
    }

    #[test]
    fn tokens_new_to_old_same() {
        crate::log_init();

        // arrange
        let input = "2 + x^2 - (3x^2 - 7y / sin[z])";

        // act
        let tokens = lex(input).expect("Must be a valid input").1;
        let new_tokens = StrTokenParser::<Token>::new(input)
            .inspect(|new_token| trace!(?new_token))
            .map(|p| {
                p.expect("Should not encounter errors")
                    .try_into()
                    .expect("Should not have unsupported tokens")
            })
            .collect::<Vec<_>>();

        // assert
        assert_eq!(tokens, new_tokens);
    }
}
