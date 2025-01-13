use super::Word;

use proptest::{prelude::ProptestConfig, prop_assert_eq, proptest};

use alloc::string::String;

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 10_000,
        max_default_size_range: 200,
        ..ProptestConfig::default()
    })]

    #[test]
    fn never_panics(input: String) {
        let _ = Word::parse(&input);
    }

    #[test]
    fn parses_words(word in "[a-zA-Z_][a-zA-Z0-9_]*", sep in "[^a-zA-Z0-9_]", garbage: String) {
        let input = format!("{word}{sep}{garbage}");
        let garbage = format!("{sep}{garbage}");

        prop_assert_eq!(
            Word::parse(&input),
            Some((Word(word.as_str()), garbage.as_str()))
        );
    }

    #[test]
    fn not_parses_garbage(input in "[^a-zA-Z_]*") {
        prop_assert_eq!(
            Word::parse(&input),
            None,
        );
    }
}
