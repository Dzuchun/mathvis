use core::ops::Not;

#[derive(
    Debug,
    Clone,
    Copy,
    Hash,
    PartialEq,
    Eq,
    derive_more::AsRef,
    derive_more::Deref,
    derive_more::Display,
)]
#[display("{_0}")]
pub struct Word<W>(W);

#[inline]
fn valid_start(ch: char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '_')
}

#[inline]
fn valid_continue(ch: char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9')
}

impl<'s> Word<&'s str> {
    pub fn parse(input: &'s str) -> Option<(Self, &'s str)> {
        let mut char_inds = input
            // be a good (cat)boy and iterate over characters
            .char_indices();
        // get first non-whitespace char (if there's none, we are done)
        let (_, first_char) = char_inds.next()?;
        if !valid_start(first_char) {
            // first non-whitespace is not a valid started for word; bail
            return None;
        }
        // word will end, once we
        let end_pos = char_inds
            // find next first char that is not a valid continue
            .find_map(|(pos, ch)| valid_continue(ch).not().then_some(pos))
            // if there none, word is up to the end of string
            .unwrap_or(input.len());
        Some((Self(&input[..end_pos]), &input[end_pos..]))
    }
}

#[macro_export]
macro_rules! word {
    (? $input:ident) => {
        'word: {
            let Some((word, rest)) = $crate::lexer_v2::word::Word::parse(stringify!($input)) else {
                break 'word None;
            };
            if !rest.trim().is_empty() {
                break 'word None;
            }
            Some(word)
        }
    };
    ($input:ident) => {
        word!(?$input).expect("Invalid word")
    };
}

#[cfg(test)]
mod tests;

#[cfg(test)]
mod prop_tests;
