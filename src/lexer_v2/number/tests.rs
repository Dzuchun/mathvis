use super::{Digits, Exponent, Number};

macro_rules! test_digits_parse {
    ($name:ident, $input:literal, $expected:expr) => {
        ::paste::paste! {
            #[test]
            fn [< $name _for_digits >]() {
                $crate::log_init();

                assert_eq!(Digits::parse($input), $expected);
            }
        }
    };
}

test_digits_parse!(empty_should_fail, "", None);
test_digits_parse!(whitespace_should_fail, " ", None);
test_digits_parse!(letter_should_fail, "a", None);
test_digits_parse!(minus_should_fail, "-", None);
test_digits_parse!(minus_one_should_fail, "-1", None);

test_digits_parse!(zero_should_ok, "0", Some((Digits("0"), "")));
test_digits_parse!(p42_should_ok, "42", Some((Digits("42"), "")));
test_digits_parse!(
    number_space_should_leave_space,
    "424233 ",
    Some((Digits("424233"), " "))
);
test_digits_parse!(
    number_space_letter_should_leave_space_letter,
    "424233 l",
    Some((Digits("424233"), " l"))
);
test_digits_parse!(
    number_letter_should_leave_letter,
    "2p",
    Some((Digits("2"), "p"))
);
test_digits_parse!(
    big_number,
    "9934023759839048593537636598927402740240740720909742094----bbb",
    Some((
        Digits("9934023759839048593537636598927402740240740720909742094"),
        "----bbb"
    ))
);

macro_rules! test_exponent_parse {
    ($name:ident, $input:literal, $expected:expr) => {
        ::paste::paste! {
            #[test]
            fn [< $name _for_exponent >]() {
                $crate::log_init();

                assert_eq!(Exponent::parse($input), $expected);

            }
        }
    };
}

test_exponent_parse!(empty_should_fail, "", None);
test_exponent_parse!(minus_should_fail, "-", None);
test_exponent_parse!(number_should_fail, "423", None);
test_exponent_parse!(only_e_should_fail, "e", None);
test_exponent_parse!(only_cap_e_should_fail, "E", None);

test_exponent_parse!(
    e1_should_ok,
    "e1",
    Some((
        Exponent {
            e: "e",
            sign: None,
            digits: Digits("1"),
        },
        ""
    ))
);
test_exponent_parse!(
    emore_should_ok,
    "e92324l",
    Some((
        Exponent {
            e: "e",
            sign: None,
            digits: Digits("92324"),
        },
        "l"
    ))
);
test_exponent_parse!(
    emore_minus_should_ok,
    "e-92324l",
    Some((
        Exponent {
            e: "e",
            sign: Some("-"),
            digits: Digits("92324"),
        },
        "l"
    ))
);
test_exponent_parse!(
    emore_plus_should_ok,
    "e+92324l",
    Some((
        Exponent {
            e: "e",
            sign: Some("+"),
            digits: Digits("92324"),
        },
        "l"
    ))
);
test_exponent_parse!(emore_plus_plus_should_fail, "e++92324l", None);

test_exponent_parse!(
    emore_plus_num_plus_should_ok,
    "E+92+",
    Some((
        Exponent {
            e: "E",
            sign: Some("+"),
            digits: Digits("92")
        },
        "+"
    ))
);

macro_rules! test_number_parse {
    ($name:ident, $input:literal, $expected:expr) => {
        ::paste::paste! {
            #[test]
            fn [< $name _for_number >]() {
                $crate::log_init();

                assert_eq!(Number::parse($input), $expected);
            }
        }
    };
}

test_number_parse!(empty_should_fail, "", None);
test_number_parse!(word_shold_fail, "word", None);

test_number_parse!(
    int_should_ok,
    "1",
    Some((
        Number::Integer {
            int: Digits("1"),
            exp: None
        },
        ""
    ))
);
test_number_parse!(
    int_should_ok2,
    "99",
    Some((
        Number::Integer {
            int: Digits("99"),
            exp: None
        },
        ""
    ))
);

test_number_parse!(
    int_exp,
    "99E2",
    Some((
        Number::Integer {
            int: Digits("99"),
            exp: Some(Exponent {
                e: "E",
                sign: None,
                digits: Digits("2")
            })
        },
        ""
    ))
);

test_number_parse!(
    int_exp_minus,
    "99E-2",
    Some((
        Number::Integer {
            int: Digits("99"),
            exp: Some(Exponent {
                e: "E",
                sign: Some("-"),
                digits: Digits("2")
            })
        },
        ""
    ))
);
test_number_parse!(
    int_exp_plus,
    "99E+2",
    Some((
        Number::Integer {
            int: Digits("99"),
            exp: Some(Exponent {
                e: "E",
                sign: Some("+"),
                digits: Digits("2")
            })
        },
        ""
    ))
);
test_number_parse!(
    int_plus_plus_should_no_exp,
    "99E++2",
    Some((
        Number::Integer {
            int: Digits("99"),
            exp: None
        },
        "E++2"
    ))
);

test_number_parse!(
    dec_int_only,
    "2.",
    Some((
        Number::DecimalInt {
            int: Digits("2"),
            point: ".",
            exp: None
        },
        ""
    ))
);

test_number_parse!(
    dec_int_only_exp,
    "2.E-3-",
    Some((
        Number::DecimalInt {
            int: Digits("2"),
            point: ".",
            exp: Some(Exponent {
                e: "E",
                sign: Some("-"),
                digits: Digits("3")
            })
        },
        "-"
    ))
);

test_number_parse!(
    dec_int_full,
    "3.141592--",
    Some((
        Number::Decimal {
            int: Digits("3"),
            point: ".",
            frac: Digits("141592"),
            exp: None
        },
        "--"
    ))
);

test_number_parse!(
    dec_int_full_exp,
    "3141.592e-3+3-",
    Some((
        Number::Decimal {
            int: Digits("3141"),
            point: ".",
            frac: Digits("592"),
            exp: Some(Exponent {
                e: "e",
                sign: Some("-"),
                digits: Digits("3")
            })
        },
        "+3-"
    ))
);

test_number_parse!(
    dec_int_frac_exp,
    ".592e3+3-",
    Some((
        Number::DecimalFraction {
            point: ".",
            frac: Digits("592"),
            exp: Some(Exponent {
                e: "e",
                sign: None,
                digits: Digits("3")
            })
        },
        "+3-"
    ))
);

test_number_parse!(
    dec_int_frac,
    ".592_3+3-",
    Some((
        Number::DecimalFraction {
            point: ".",
            frac: Digits("592"),
            exp: None
        },
        "_3+3-"
    ))
);

test_number_parse!(
    dec_int_frac_exp2,
    ".592E-37",
    Some((
        Number::DecimalFraction {
            point: ".",
            frac: Digits("592"),
            exp: Some(Exponent {
                e: "E",
                sign: Some("-"),
                digits: Digits("37")
            })
        },
        ""
    ))
);
