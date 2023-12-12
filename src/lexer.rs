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
pub enum GroupingType {
    Parentheses,
    Brackets,
    Braces,
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
    GroupOpen(GroupingType),
    GroupClose(GroupingType),
    Operator(Operator),
    Comma,
    ImaginaryUnit,
    Number(f64),
    Identifier(String),
}

#[derive(Debug, PartialEq, derive_more::Display)]
pub enum TokenType {
    Grouping,
    Operator,
    Comma,
    Constant,
    Identifier,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::GroupOpen(t) => write!(
                f,
                "{}",
                match t {
                    GroupingType::Parentheses => '(',
                    GroupingType::Brackets => '[',
                    GroupingType::Braces => '{',
                }
            ),
            Token::GroupClose(t) => write!(
                f,
                "{}",
                match t {
                    GroupingType::Parentheses => ')',
                    GroupingType::Brackets => ']',
                    GroupingType::Braces => '}',
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
            Token::Identifier(n) => write!(f, "{}", n),
        }
    }
}

/*
impl Token {
    pub fn description(&self) -> &'static str {
        match self {
            Token::GroupOpen(t) => match t {
                GroupingType::Parentheses => "opening paren",
                GroupingType::Brackets => "opening bracket",
                GroupingType::Braces => "opening brace",
            },
            Token::GroupClose(t) => match t {
                GroupingType::Parentheses => "closing paren",
                GroupingType::Brackets => "closing bracket",
                GroupingType::Braces => "closing brace",
            },
            Token::Operator(o) => match o {
                Operator::Plus => "plus",
                Operator::Minus => "minus",
                Operator::Star => "star",
                Operator::Slash => "slash",
                Operator::Cap => "cap",
            },
            Token::Comma => "comma",
            Token::ImaginaryUnit => "imaginary unit",
            Token::Number(_) => "number literal",
            Token::Identifier(_) => "identifier",
        }
    }
}
*/

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
    use crate::lexer::GroupingType;

    use super::lex;
    use super::Token;

    #[test]
    fn tokens() {
        // arrange
        let input = r#"2 + x^2 - (3x^2 - 7y / sin[z])"#;

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
}
