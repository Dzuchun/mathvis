use super::{number::Number, word::Word, Parseable};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct SpannedToken<Span, D = &'static str, C = &'static str, W = &'static str> {
    token: Token<D, C, W>,
    span: Span,
}

impl<'s> Parseable<'s> for SpannedToken<&'s str, &'s str, &'s str, &'s str> {
    #[inline]
    fn try_parse(input: &'s str) -> Option<(Self, &'s str)> {
        Token::try_parse(input).map(|(token, rest)| {
            (
                Self {
                    token,
                    span: &input[..input.len() - rest.len()],
                },
                rest,
            )
        })
    }
}

/// Grouping symbol kind
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
pub enum GroupingKind {
    #[display("paren")]
    Parentheses,
    #[display("bracket")]
    Brackets,
    #[display("brace")]
    Braces,
    #[display("angle")]
    Angle,
}

/// Grouping symbol kind
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
pub enum GroupingSide {
    #[display("left")]
    Left,
    #[display("right")]
    Right,
}

/// Represents operators (not necessary corresponding to actual [`crate::evaluation_tree::Operator`]s).
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
pub enum SpecialKind {
    #[display(".")]
    Dot,
    #[display(",")]
    Comma,
    #[display("+")]
    Plus,
    #[display("-")]
    Minus,
    #[display("*")]
    Star,
    #[display("/")]
    Slash,
    #[display("%")]
    Percent,
    #[display("^")]
    Cap,
    #[display("|")]
    Pipe,
    #[display("&")]
    Umpersand,
    #[display(":")]
    Colon,
    #[display("!")]
    Bang,
    #[display("=")]
    Equals,
    #[display("?")]
    Question,
    #[display("@")]
    At,
    #[display("~")]
    Tilda,
}

/// Represents a single token.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
pub enum Token<C = &'static str, D = &'static str, W = &'static str> {
    #[display("{kind}_{side}")]
    Grouping {
        kind: GroupingKind,
        side: GroupingSide,
    },
    #[display("{_0}")]
    Special(SpecialKind),
    #[display("{_0}")]
    Number(Number<D, C>),
    #[display("{_0}")]
    Word(Word<W>),
}

impl<'s> Parseable<'s> for Token<&'s str, &'s str, &'s str> {
    fn try_parse(input: &'s str) -> Option<(Self, &'s str)> {
        // if there's no char, there's no token
        let c = input.chars().next()?; // (short-circuit)

        // try parsing word
        //
        // can't be confused with any other token, and mot of the tokens are expected to be words
        if let Some((word, rest)) = Word::parse(input) {
            return Some((Token::word(word), rest));
        }

        // try parsing number
        //
        // (same idea)
        if let Some((number, rest)) = Number::parse(input) {
            return Some((Token::number(number), rest));
        }

        // token is not a number and not a word, so it is a one-character token
        let token = match c {
            '(' => Token::left_paren(),
            ')' => Token::right_paren(),
            '[' => Token::left_bracket(),
            ']' => Token::right_bracket(),
            '{' => Token::left_brace(),
            '}' => Token::right_brace(),
            '.' => Token::dot(),
            ',' => Token::comma(),
            '+' => Token::plus(),
            '-' => Token::minus(),
            '*' => Token::star(),
            '/' => Token::slash(),
            '%' => Token::percent(),
            '^' => Token::cap(),
            '|' => Token::pipe(),
            '&' => Token::umpersand(),
            ':' => Token::colon(),
            '!' => Token::bang(),
            '=' => Token::equals(),
            '?' => Token::question(),
            '@' => Token::at(),
            '~' => Token::tilda(),
            // not a token
            _ => return None,
        };

        let next_pos = input
            .char_indices()
            // position or the char after first
            .nth(1)
            // or end of the line, if that's the only char
            .map_or(input.len(), |(pos, _)| pos);

        Some((token, &input[next_pos..]))
    }
}

// helper constructors and macro
impl<C, D, W> Token<C, D, W> {
    pub fn dot() -> Self {
        Token::Special(SpecialKind::Dot)
    }

    pub fn comma() -> Self {
        Token::Special(SpecialKind::Comma)
    }

    pub fn left_paren() -> Self {
        Self::Grouping {
            kind: GroupingKind::Parentheses,
            side: GroupingSide::Left,
        }
    }

    pub fn right_paren() -> Self {
        Self::Grouping {
            kind: GroupingKind::Parentheses,
            side: GroupingSide::Right,
        }
    }

    pub fn left_bracket() -> Self {
        Self::Grouping {
            kind: GroupingKind::Brackets,
            side: GroupingSide::Left,
        }
    }

    pub fn right_bracket() -> Self {
        Self::Grouping {
            kind: GroupingKind::Brackets,
            side: GroupingSide::Right,
        }
    }

    pub fn left_brace() -> Self {
        Self::Grouping {
            kind: GroupingKind::Braces,
            side: GroupingSide::Left,
        }
    }

    pub fn right_brace() -> Self {
        Self::Grouping {
            kind: GroupingKind::Braces,
            side: GroupingSide::Right,
        }
    }

    pub fn left_angle() -> Self {
        Self::Grouping {
            kind: GroupingKind::Angle,
            side: GroupingSide::Left,
        }
    }

    pub fn right_angle() -> Self {
        Self::Grouping {
            kind: GroupingKind::Angle,
            side: GroupingSide::Right,
        }
    }

    pub fn plus() -> Self {
        Token::Special(SpecialKind::Plus)
    }

    pub fn minus() -> Self {
        Token::Special(SpecialKind::Minus)
    }

    pub fn star() -> Self {
        Token::Special(SpecialKind::Star)
    }

    pub fn slash() -> Self {
        Token::Special(SpecialKind::Slash)
    }

    pub fn percent() -> Self {
        Token::Special(SpecialKind::Percent)
    }

    pub fn cap() -> Self {
        Token::Special(SpecialKind::Cap)
    }

    pub fn pipe() -> Self {
        Token::Special(SpecialKind::Pipe)
    }

    pub fn umpersand() -> Self {
        Token::Special(SpecialKind::Umpersand)
    }

    pub fn colon() -> Self {
        Token::Special(SpecialKind::Colon)
    }

    pub fn bang() -> Self {
        Token::Special(SpecialKind::Bang)
    }

    pub fn equals() -> Self {
        Token::Special(SpecialKind::Equals)
    }

    pub fn question() -> Self {
        Token::Special(SpecialKind::Question)
    }

    pub fn at() -> Self {
        Token::Special(SpecialKind::At)
    }

    pub fn tilda() -> Self {
        Token::Special(SpecialKind::Tilda)
    }

    pub fn number(n: Number<D, C>) -> Self {
        Token::Number(n)
    }

    pub fn word(w: Word<W>) -> Self {
        Token::Word(w)
    }
}

#[macro_export]
macro_rules! tokens_inner {
    () => {
        arr![]
    };
    ( ( $($inner:tt)* ) $($rest:tt)* ) => {
        Concat::concat(
            Concat::concat(
                Concat::concat(
                    arr![Token::left_paren()],
                    tokens_inner!($($inner)*)
                ),
                arr![Token::right_paren()],
            ),
            tokens_inner!($($rest)*)
        )
    };
    ( [ $($inner:tt)* ] $($rest:tt)* ) => {
        Concat::concat(
            Concat::concat(
                Concat::concat(
                    arr![Token::left_bracket()],
                    tokens_inner!($($inner)*)
                ),
                arr![Token::right_bracket()],
            ),
            tokens_inner!($($rest)*)
        )
    };
    ( { $($inner:tt)* } $($rest:tt)* ) => {
        Concat::concat(
            Concat::concat(
                Concat::concat(
                    arr![Token::left_brace()],
                    tokens_inner!($($inner)*)
                ),
                arr![Token::right_brace()],
            ),
            tokens_inner!($($rest)*)
        )
    };
    (. $($rest:tt)*) => {
        Concat::concat(
            arr![Token::dot()],
            tokens_inner!($($rest)*)
        )
    };
    (, $($rest:tt)*) => {
        Concat::concat(
            arr![Token::comma()],
            tokens_inner!($($rest)*)
        )
    };
    (+ $($rest:tt)*) => {
        Concat::concat(
            arr![Token::plus()],
            tokens_inner!($($rest)*)
        )
    };
    (- $($rest:tt)*) => {
        Concat::concat(
            arr![Token::minus()],
            tokens_inner!($($rest)*)
        )
    };
    (* $($rest:tt)*) => {
        Concat::concat(
            arr![Token::star()],
            tokens_inner!($($rest)*)
        )
    };
    (/ $($rest:tt)*) => {
        Concat::concat(
            arr![Token::slash()],
            tokens_inner!($($rest)*)
        )
    };
    (% $($rest:tt)*) => {
        Concat::concat(
            arr![Token::percent()],
            tokens_inner!($($rest)*)
        )
    };
    (^ $($rest:tt)*) => {
        Concat::concat(
            arr![Token::cap()],
            tokens_inner!($($rest)*)
        )
    };
    (| $($rest:tt)*) => {
        Concat::concat(
            arr![Token::pipe()],
            tokens_inner!($($rest)*)
        )
    };
    (& $($rest:tt)*) => {
        Concat::concat(
            arr![Token::umpersand()],
            tokens_inner!($($rest)*)
        )
    };
    (: $($rest:tt)*) => {
        Concat::concat(
            arr![Token::colon()],
            tokens_inner!($($rest)*)
        )
    };
    (! $($rest:tt)*) => {
        Concat::concat(
            arr![Token::bang()],
            tokens_inner!($($rest)*)
        )
    };
    (= $($rest:tt)*) => {
        Concat::concat(
            arr![Token::equals()],
            tokens_inner!($($rest)*)
        )
    };
    (? $($rest:tt)*) => {
        Concat::concat(
            arr![Token::question()],
            tokens_inner!($($rest)*)
        )
    };
    (@ $($rest:tt)*) => {
        Concat::concat(
            arr![Token::at()],
            tokens_inner!($($rest)*)
        )
    };
    (~ $($rest:tt)*) => {
        Concat::concat(
            arr![Token::tilda()],
            tokens_inner!($($rest)*)
        )
    };
    (< $($rest:tt)*) => {
        Concat::concat(
            arr![Token::left_angle()],
            tokens_inner!($($rest)*)
        )
    };
    (> $($rest:tt)*) => {
        Concat::concat(
            arr![Token::right_angle()],
            tokens_inner!($($rest)*)
        )
    };
    ($token:ident $($rest:tt)*) => {
        Concat::concat(
            arr![Token::word(word!($token))],
            tokens_inner!($($rest)*)
        )
    };
    ($token:literal $($rest:tt)*) => {
        Concat::concat(
            arr![Token::number(number!($token))],
            tokens_inner!($($rest)*)
        )
    };
}

#[macro_export]
macro_rules! tokens {
    ($($rest:tt)*) => {{
        #[allow(unused_imports)]
        use generic_array::{arr, sequence::Concat};
        #[allow(unused_imports)]
        use $crate::{lexer_v2::token::Token, word, tokens_inner, number};
        tokens_inner!($($rest)*)
    }};
}

#[cfg(test)]
mod tests;
