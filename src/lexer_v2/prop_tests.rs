use super::{token::SpannedToken, StrTokenParser};

use proptest::{prelude::ProptestConfig, proptest};

use alloc::string::String;

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 30_000,
        max_default_size_range: 500,
        ..ProptestConfig::default()
    })]

    #[test]
    fn parser_never_panics(input: String) {
        let parser = StrTokenParser::<'_, SpannedToken<&str>>::new(&input);
        let _ = parser.count();
    }
}
