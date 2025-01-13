use core::ops::Not;

use super::{Digits, Exponent, Number};

use proptest::{prelude::ProptestConfig, prop_assert_eq, proptest};

use alloc::string::String;

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 10_000,
        max_default_size_range: 200,
        ..ProptestConfig::default()
    })]
    #[test]
    fn digits_never_panics(input: String) {
        let _ = Digits::parse(&input);
    }

    #[test]
    fn digits_never_parse_letters(s in "[^0-9]+") {
        prop_assert_eq!(Digits::parse(&s), None);
    }

    #[test]
    fn digits_always_parse_digits(digits in "[0-9]+", garbage in "[^0-9]+") {
        let input = format!("{digits}{garbage}");
        prop_assert_eq!(Digits::parse(&input), Some((Digits(digits.as_str()), garbage.as_str())));
    }

    #[test]
    fn exponent_never_panics(input: String) {
        let _ = Exponent::parse(&input);
    }

    #[test]
    fn exponent_valid_parses(e in "[eE]", sign in "[+-]?", digits in "[0-9]+", sep in "[^0-9]", garbage: String) {
        let input = format!("{e}{sign}{digits}{sep}{garbage}");
        let garbage = format!("{sep}{garbage}");
        prop_assert_eq!(
            Exponent::parse(&input),
            Some((
                Exponent{
                    e: e.as_str(),
                    sign: sign.is_empty().not().then_some(sign.as_str()),
                    digits: Digits(digits.as_str())
                },
                garbage.as_str())
            ));
    }

    #[test]
    fn number_never_panics(input: String) {
        let _ = Number::parse(&input);
    }

    #[test]
    fn number_parses_ints(digits in "[0-9]+", sep in "[^eE.0-9]", garbage: String) {
        let input = format!("{digits}{sep}{garbage}");
        let garbage = format!("{sep}{garbage}");
        prop_assert_eq!(
            Number::parse(&input),
            Some((Number::Integer { int: Digits(digits.as_str()), exp: None }, garbage.as_str()))
        );
    }

    #[test]
    fn number_parses_doubles(mut part1 in "[0-9]+", mut part2 in "[0-9]*", swap_parts: bool, e in "[eE]", exponent_sign in "[+-]?", exponent in "[0-9]+", sep in "[^0-9]", garbage: String) {
        if swap_parts {
            core::mem::swap(&mut part1, &mut part2);
        }
        let input = format!("{part1}.{part2}{e}{exponent_sign}{exponent}{sep}{garbage}");
        let garbage = format!("{sep}{garbage}");
        let expected = match(part1.is_empty(), part2.is_empty()) {
            (false, false) => Number::Decimal {
                int: Digits(part1.as_str()),
                point: ".",
                frac: Digits(part2.as_str()),
                exp: Some(Exponent {
                    e: e.as_str(),
                    sign: exponent_sign.is_empty().not().then_some(exponent_sign.as_str()),
                    digits: Digits(exponent.as_str())
                })
            },
            (true, false) => Number::DecimalFraction {
                point: ".",
                frac: Digits(part2.as_str()),
                exp: Some(Exponent {
                    e: e.as_str(),
                    sign: exponent_sign.is_empty().not().then_some(exponent_sign.as_str()),
                    digits: Digits(exponent.as_str())
                })
            },
            (false, true) => Number::DecimalInt {
                int: Digits(part1.as_str()),
                point: ".",
                exp: Some(Exponent {
                    e: e.as_str(),
                    sign: exponent_sign.is_empty().not().then_some(exponent_sign.as_str()),
                    digits: Digits(exponent.as_str())
                })
            },
            (true, true) => unreachable!(),
        };
        prop_assert_eq!(
            Number::parse(&input),
            Some((
                expected,
                garbage.as_str()
            ))
        );
    }

    #[test]
    fn not_parses_garbage(input in "[^0-9.]") {
        prop_assert_eq!(
            Number::parse(&input),
            None,
        );
    }
}
