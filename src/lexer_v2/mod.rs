use core::marker::PhantomData;

pub mod number;
pub mod token;
pub mod word;

pub trait TokenInput {
    fn is_empty(&self) -> bool;
    fn on_invalid(self) -> Self;
}

pub trait Parseable<'s>: Sized + 's {
    fn try_parse(input: &'s str) -> Option<(Self, &'s str)>;
}

impl<'s, T> StrTokenParser<'s, T> {
    pub fn new(input: &'s str) -> Self {
        Self {
            input,
            _phantom: PhantomData,
        }
    }
}

pub struct StrTokenParser<'s, T> {
    input: &'s str,
    _phantom: PhantomData<T>,
}

fn skip_err(s: &str) -> (&str, &str) {
    let next_whitespace = s
        .char_indices()
        .find_map(|(pos, ch)| ch.is_whitespace().then_some(pos))
        .unwrap_or(s.len());
    s.split_at(next_whitespace)
}

impl<'s, T: Parseable<'s>> Iterator for StrTokenParser<'s, T> {
    type Item = Result<T, &'s str>;

    #[tracing::instrument(level = "trace", skip(self), name = "token_parser", fields(input = self.input))]
    fn next(&mut self) -> Option<Self::Item> {
        let ws = self.input.trim_start();
        trace!(ws);
        if ws.is_empty() {
            // no more tokens, exhausted the input
            return None;
        }

        if let Some((token, rest)) = T::try_parse(ws) {
            // successful token parse, return the token
            trace!("Parsed token");
            self.input = rest;
            Some(Ok(token))
        } else {
            // failed to parse the token
            //
            // skip to next whitespace character
            let (erroneous, new_input) = skip_err(ws);
            self.input = new_input;
            trace!(erroneous);
            Some(Err(erroneous))
        }
    }
}

#[cfg(test)]
mod prop_tests;
