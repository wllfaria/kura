use miette::{Error, LabeledSpan};

mod token;

use token::{NumSize, Token};

struct Lexer<'lex> {
    pos: usize,
    source: &'lex str,
    complete_source: &'lex str,
}

impl<'lex> Lexer<'lex> {
    pub fn new(source: &'lex str) -> Self {
        Self {
            pos: 0,
            source,
            complete_source: source,
        }
    }
}

impl<'lex> Lexer<'lex> {
    fn lex_numerals(&mut self) -> Result<Token<'lex>, Error> {
        let end_of_numeral = self
            .source
            .find(|c| !matches!(c, '-' | '_' | '.' | '0'..='9'))
            .unwrap_or(self.source.len());

        let mut literal = &self.source[..end_of_numeral];

        let mut dotted = literal.splitn(3, '.');
        match (dotted.next(), dotted.next(), dotted.next()) {
            (Some(one), Some(two), Some(_)) => {
                // numeric literal contains two or more dots, so we only lex
                // from the first numerical to the last numerical before the
                // second dot, <one.len()>.<two.len()>
                //
                // 123.456.789 -> 123.456 (leave .789) to be lexed after
                literal = &literal[..one.len() + 1 + two.len()];
            }
            _ => {
                // literal is either a integer, or a valid floating pointer
                // numeral, so we don't have to do anything here
            }
        }

        let mut dashed = literal.splitn(3, '-');
        match (dashed.next(), dashed.next(), dashed.next()) {
            (Some(""), Some(two), Some(_)) => {
                // numeric literal contain two or more dashes, but one of the
                // dashes is the first character, meaning we should only lex the
                // middle part as a signed integer
                //
                // -123.456-123 ("", "123.456", "123") (result of splitn)
                literal = &literal[..two.len() + 1]; // +1 to account for the `-`
            }
            (Some(one), Some(_), Some(_)) => {
                // numeric literal has two or more dashes, and don't start with a
                // dash, so we only parse the first part and ignore the rest.
                //
                // 123-456.78-9 ("123", "456.78", "9")
                literal = &literal[..one.len()];
            }
            _ => {
                // numeral is a unsigned integer, so we don't have to do
                // anything here
            }
        }

        // NOTE: we have to update the lexer state here before we potentially
        // change the size of the literal after trimming underscores
        let bytes_eaten = literal.len();
        self.pos += bytes_eaten;
        self.source = &self.source[bytes_eaten..];

        // we accept underscores in numerals, but rust parser don't, so we
        // trim here before parsing to rust's parser
        let literal = literal.replace('_', "");

        // numbers are allowed to potentially have a postfix specifying its size
        // like `1234usize`
        let end_of_postfix = self
            .source
            .find(|c: char| c.is_whitespace())
            .unwrap_or(self.source.len());

        let postfix = &self.source[..end_of_postfix];
        let size = match NumSize::try_from(postfix) {
            Ok(size) => Some(size),
            Err(_) => None,
        };

        if size.is_some() {
            self.source = &self.source[postfix.len()..];
            self.pos += postfix.len();
        }

        let is_signed = literal.contains('-');
        let is_float = literal.contains('.');
        match (is_float, is_signed) {
            (false, true) => Ok(Token::SignedInteger {
                value: match literal.parse() {
                    Ok(numeral) => numeral,
                    Err(e) => return Err(miette::miette! {
                        labels = vec![
                            LabeledSpan::at(self.pos - bytes_eaten..self.pos, "this numeric literal"),
                        ],
                        "{e}",
                    }.with_source_code(self.complete_source.to_string())),
                },
                size,
            }),
            (false, false) => Ok(Token::Integer {
                value: match literal.parse() {
                    Ok(numeral) => numeral,
                    Err(e) => return Err(miette::miette! {
                        labels = vec![
                            LabeledSpan::at(self.pos - bytes_eaten..self.pos, "this numeric literal"),
                        ],
                        "{e}",
                    }.with_source_code(self.complete_source.to_string())),
                },
                size,
            }),
            (true, _) => Ok(Token::Float {
                value: match literal.parse() {
                    Ok(numeral) => numeral,
                    Err(e) => return Err(miette::miette! {
                        labels = vec![
                            LabeledSpan::at(self.pos - bytes_eaten..self.pos, "this numeric literal"),
                        ],
                        "{e}",
                    }.with_source_code(self.complete_source.to_string())),
                },
                size,
            }),
        }
    }
}

impl<'lex> Iterator for Lexer<'lex> {
    type Item = Result<Token<'lex>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let mut chars = self.source.chars().peekable();
            let c = chars.next()?;

            match c {
                c if c.is_whitespace() => {
                    self.source = &self.source[c.len_utf8()..];
                    self.pos += c.len_utf8();
                    continue;
                }
                c if c.is_numeric() => return Some(self.lex_numerals()),
                '-' => match chars.peek() {
                    Some(c) if c.is_numeric() => return Some(self.lex_numerals()),
                    _ => todo!(),
                },
                _ => todo!(),
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_sut(source: &str) -> Lexer<'_> {
        Lexer::new(source)
    }

    #[test]
    fn lexing_numericals() {
        let source = [
            // positive integers and floats
            "12346.67890",
            "1234567890",
            "9__876___5.43____210",
            "9_876_5.43_210",
            "9_____876__543____210",
            "9_876_543_210",
            // negative integers and floats
            "-9_____876__5.43____210",
            "-123_46.678_90",
            "-12346.67890",
            "-1234567890",
            "-9_____876__543____210",
            "-9_876_543_210",
            "-9_876_-5-43_210",
            // integers and floats with postfixes
            "-123456789i32",
            "123456789usize",
            "123.456789f64",
            "123.456789f64",
            "-12f8",
        ];
        let source = source.join("\n");

        let mut all_tokens = vec![];

        for token in make_sut(&source) {
            all_tokens.push(token.unwrap());
        }

        insta::assert_debug_snapshot!(all_tokens);
    }
}
