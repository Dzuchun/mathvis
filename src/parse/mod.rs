use std::fmt::Display;

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

type Res<'l, T = Token> = nom::IResult<&'l str, T, Error<&'l str>>;

#[derive(Debug, PartialEq, Clone)]
pub enum ParenType {
    Paren,
    Bracket,
    Brace,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    Cap,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Open(ParenType),
    Close(ParenType),
    Operator(Operator),
    Comma,
    ImaginaryUnit,
    Number(f64),
    Ident(String),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Open(t) => write!(
                f,
                "{}",
                match t {
                    ParenType::Paren => '(',
                    ParenType::Bracket => '[',
                    ParenType::Brace => '{',
                }
            ),
            Token::Close(t) => write!(
                f,
                "{}",
                match t {
                    ParenType::Paren => ')',
                    ParenType::Bracket => ']',
                    ParenType::Brace => '}',
                }
            ),
            Token::Operator(o) => write!(
                f,
                "{}",
                match o {
                    Operator::Plus => '+',
                    Operator::Minus => '-',
                    Operator::Star => '*',
                    Operator::Slash => '/',
                    Operator::Cap => '^',
                }
            ),
            Token::Comma => write!(f, ","),
            Token::ImaginaryUnit => write!(f, "i"),
            Token::Number(v) => write!(f, "{}", v),
            Token::Ident(n) => write!(f, "{}", n),
        }
    }
}

impl Token {
    pub fn description(&self) -> &'static str {
        match self {
            Token::Open(t) => match t {
                ParenType::Paren => "opening paren",
                ParenType::Bracket => "opening bracket",
                ParenType::Brace => "opening brace",
            },
            Token::Close(t) => match t {
                ParenType::Paren => "closing paren",
                ParenType::Bracket => "closing bracket",
                ParenType::Brace => "closing brace",
            },
            Token::Operator(o) => match o {
                Operator::Plus => "plus operator",
                Operator::Minus => "minus operator",
                Operator::Star => "star operator",
                Operator::Slash => "slash operator",
                Operator::Cap => "cap operator",
            },
            Token::Comma => "comma",
            Token::ImaginaryUnit => "imaginary unit",
            Token::Number(_) => "number literal",
            Token::Ident(_) => "identifier",
        }
    }
}

fn paren_open(i: &str) -> Res {
    map(one_of("([{"), |c| {
        Token::Open(match c {
            '(' => ParenType::Paren,
            '[' => ParenType::Bracket,
            '{' => ParenType::Brace,
            _ => unreachable!(),
        })
    })(i)
}

fn paren_close(i: &str) -> Res {
    map(one_of(")]}"), |c| {
        Token::Close(match c {
            ')' => ParenType::Paren,
            ']' => ParenType::Bracket,
            '}' => ParenType::Brace,
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
fn ident(i: &str) -> Res {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
        |name: &str| Token::Ident(name.to_owned()),
    )(i)
}

fn number(i: &str) -> Res {
    map(double, Token::Number)(i)
}

fn whitespace(i: &str) -> Res<()> {
    Ok((many0(one_of(" \t\x0c\n"))(i)?.0, ()))
}

pub fn parse(i: &str) -> Res<Vec<Token>> {
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
                ident,
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
    use crate::parse::ParenType;

    use super::parse;
    use super::Token;

    #[test]
    fn tokens() {
        // arrange
        let input = r#"2 + x^2 - (3x^2 - 7y / sin[z])"#;

        // act
        let tokens = parse(input);

        // assert
        assert_eq!(
            tokens,
            Ok((
                "",
                vec![
                    Token::Number(2f64),
                    Token::Operator(crate::parse::Operator::Plus),
                    Token::Ident("x".to_owned()),
                    Token::Operator(crate::parse::Operator::Cap),
                    Token::Number(2f64),
                    Token::Operator(crate::parse::Operator::Minus),
                    Token::Open(ParenType::Paren),
                    Token::Number(3f64),
                    Token::Ident("x".to_owned()),
                    Token::Operator(crate::parse::Operator::Cap),
                    Token::Number(2f64),
                    Token::Operator(crate::parse::Operator::Minus),
                    Token::Number(7f64),
                    Token::Ident("y".to_owned()),
                    Token::Operator(crate::parse::Operator::Slash),
                    Token::Ident("sin".to_owned()),
                    Token::Open(ParenType::Bracket),
                    Token::Ident("z".to_owned()),
                    Token::Close(ParenType::Bracket),
                    Token::Close(ParenType::Paren),
                ]
            ))
        );
    }
}
