use super::super::StrTokenParser;
use super::Token;
use crate::{number, word};

macro_rules! test_tokens {
    ($(#[$($attr:tt)+])* $name:ident, ($($input:tt)*), [$($expected:expr),*]) => {
        ::paste::paste! {
            #[test]
            $(
            #[$($attr)*]
            )*
            fn [< $name _tokens >]() {
                $crate::log_init();

                let input = tokens!($($input)*);
                let expected = generic_array::GenericArray::<Token<&'static str, &'static str, &'static str>, _>::from_array([$($expected),*]);

                assert_eq!(input, expected);
            }
        }
    };
}

test_tokens!(empty, (), []);

test_tokens!(dot, (.), [Token::dot()]);
test_tokens!(comma, (,), [Token::comma()]);
test_tokens!(plus, (+), [Token::plus()]);
test_tokens!(minus, (-), [Token::minus()]);
test_tokens!(star, (*), [Token::star()]);
test_tokens!(slash, (/), [Token::slash()]);
test_tokens!(percent, (%), [Token::percent()]);
test_tokens!(cap, (^), [Token::cap()]);
test_tokens!(pipe, (|), [Token::pipe()]);
test_tokens!(umpersand, (&), [Token::umpersand()]);
test_tokens!(colon, (:), [Token::colon()]);
test_tokens!(bang, (!), [Token::bang()]);
test_tokens!(equals, (=), [Token::equals()]);
test_tokens!(question, (?), [Token::question()]);
test_tokens!(at, (@), [Token::at()]);
test_tokens!(tilda, (~), [Token::tilda()]);
test_tokens!(left_angle, (<), [Token::left_angle()]);
test_tokens!(right_angle, (>), [Token::right_angle()]);

test_tokens!(word, (word), [Token::word(word!(word))]);
test_tokens!(word2, (w0r3d), [Token::word(word!(w0r3d))]);
test_tokens!(
    long_word,
    (SOME_REALLY_LONG_WORD_1111111),
    [Token::word(word!(SOME_REALLY_LONG_WORD_1111111))]
);
test_tokens!(
    #[should_panic = "Invalid word"]
    invalid_word,
    (r#not_a_valid_word),
    [Token::word(word!(not_a_valid_word))]
);

test_tokens!(number, (1), [Token::number(number!(1))]);
test_tokens!(number2, (1.), [Token::number(number!(1.))]);
test_tokens!(number3, (1.), [Token::number(number!(1.))]);
test_tokens!(number4, (1.0E0), [Token::number(number!(1.0E0))]);

test_tokens!(
    negative_number,
    (-24.003E6),
    [Token::minus(), Token::number(number!(24.003E6))]
);

test_tokens!(
    very_much_positive_number,
    (+ + +   42.0E-5),
    [Token::plus(), Token::plus(), Token::plus(), Token::number(number!(42.0E-5))]
);

test_tokens!(
    random_expr,
    (3 + 4 - 6 + twelve - 00 - ONLY),
    [
        Token::number(number!(3)),
        Token::plus(),
        Token::number(number!(4)),
        Token::minus(),
        Token::number(number!(6)),
        Token::plus(),
        Token::word(word!(twelve)),
        Token::minus(),
        Token::number(number!(00)),
        Token::minus(),
        Token::word(word!(ONLY))
    ]
);

test_tokens!(
    parens,
    ((2 + 3.4e-21) | [a - _BB45] {} / 10),
    [
        Token::left_paren(),
        Token::number(number!(2)),
        Token::plus(),
        Token::number(number!(3.4e-21)),
        Token::right_paren(),
        Token::pipe(),
        Token::left_bracket(),
        Token::word(word!(a)),
        Token::minus(),
        Token::word(word!(_BB45)),
        Token::right_bracket(),
        Token::left_brace(),
        Token::right_brace(),
        Token::slash(),
        Token::number(number!(10))
    ]
);

macro_rules! test_token_parser {
    ($name:ident, $input:literal, [$($token:expr),*$(,)?]) => {
        ::paste::paste! {
            #[test]
            fn [< $name _tokens_iter >]() {
                $crate::log_init();

                let iter = StrTokenParser::<'_, Token>::new($input);

                let parsed_tokens = iter.collect::<Vec<_>>();

                assert_eq!(parsed_tokens.as_slice(), &[$($token),*]);
            }
        }
    };
}

test_token_parser!(empty_parses, "", []);
test_token_parser!(plus_pares, "+", [Ok(Token::plus())]);
test_token_parser!(
    expr_parses,
    "+3 - b4_A[AA/ #iim 3e-67+ (17.00E-67c ]}- an mee#34\nb) \t ---",
    [
        Ok(Token::plus()),
        Ok(Token::number(number!(3))),
        Ok(Token::minus()),
        Ok(Token::word(word!(b4_A))),
        Ok(Token::left_bracket()),
        Ok(Token::word(word!(AA))),
        Ok(Token::slash()),
        Err("#iim"),
        Ok(Token::number(number!(3e-67))),
        Ok(Token::plus()),
        Ok(Token::left_paren()),
        Ok(Token::number(number!(17.00E-67))),
        Ok(Token::word(word!(c))),
        Ok(Token::right_bracket()),
        Ok(Token::right_brace()),
        Ok(Token::minus()),
        Ok(Token::word(word!(an))),
        Ok(Token::word(word!(mee))),
        Err("#34"),
        Ok(Token::word(word!(b))),
        Ok(Token::right_paren()),
        Ok(Token::minus()),
        Ok(Token::minus()),
        Ok(Token::minus()),
    ]
);
