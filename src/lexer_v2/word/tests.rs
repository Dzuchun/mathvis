use super::Word;

macro_rules! test_word_parse {
    ($name:ident, $input:literal, $expected:expr) => {
        #[test]
        fn $name() {
            assert_eq!(Word::parse($input), $expected);
        }
    };
}

test_word_parse!(empty_should_fail, "", None);
test_word_parse!(number_should_fail, "1", None);
test_word_parse!(whitespace_should_fail, " ", None);
test_word_parse!(non_ascii_should_fail, "слово", None);
test_word_parse!(minus_should_fail, "-", None);

test_word_parse!(letter_should_ok, "a", Some((Word("a"), "")));
test_word_parse!(letter_should_ok2, "U", Some((Word("U"), "")));
test_word_parse!(underscore_should_ok, "_", Some((Word("_"), "")));
test_word_parse!(
    letter_space_should_leave_space,
    "a ",
    Some((Word("a"), " "))
);
test_word_parse!(
    letter_something_should_keep_something,
    "a something",
    Some((Word("a"), " something"))
);

test_word_parse!(
    a_minus_b_should_leave_minus_b,
    "a-b",
    Some((Word("a"), "-b"))
);
