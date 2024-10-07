pub mod error;
pub mod token;

use error::Error;
use token::{FloatSizes, IntSizes, IntoToken, Kind, Operator, Primitive, Token, UIntSizes};

pub trait TransposeRef<'a, T, E: std::error::Error> {
    fn transpose(self) -> Result<Option<&'a T>, &'a E>;
}

impl<'lex> TransposeRef<'lex, Token<'lex>, Error> for Option<&'lex Result<Token<'lex>, Error>> {
    fn transpose(self) -> Result<Option<&'lex Token<'lex>>, &'lex Error> {
        match self {
            Some(result) => match result {
                Ok(token) => Ok(Some(token)),
                Err(e) => Err(e),
            },
            None => Ok(None),
        }
    }
}

pub struct Lexer<'lex> {
    pos: usize,
    source: &'lex str,
    pub complete_source: &'lex str,
    peeked: Option<Result<Token<'lex>, Error>>,
}

impl<'lex> Lexer<'lex> {
    pub fn new(source: &'lex str) -> Self {
        Self {
            pos: 0,
            source,
            complete_source: source,
            peeked: None,
        }
    }

    pub fn source_code(&self) -> &str {
        self.complete_source
    }

    pub fn peek(&mut self) -> Option<&Result<Token<'lex>, Error>> {
        if self.peeked.is_none() {
            self.peeked = self.next();
        }

        self.peeked.as_ref()
    }

    pub fn is_empty(&mut self) -> bool {
        self.peek().is_none()
    }

    fn make_token<T>(&mut self, tokenizable: T, size: usize) -> Token<'lex>
    where
        T: IntoToken<'lex>,
    {
        let start_byte = self.pos;
        self.advance_by(size);
        tokenizable.into_token(start_byte, self.pos)
    }

    pub fn expect(&mut self, expected: Kind<'_>) -> Result<Token<'lex>, Error> {
        let Some(token) = self.next().transpose()? else {
            let location = self.complete_source.len() - 1..self.complete_source.len();
            return Err(Error::from(location));
        };
        let kind = &token.kind;

        if kind == &expected {
            Ok(token)
        } else {
            Err(Error::from(token.location))
        }
    }

    pub fn expect_one_of(&mut self, expected_list: &[Kind<'_>]) -> Result<Token<'lex>, Error> {
        let token = self.next().transpose()?;
        let kind = token.as_ref().map(|token| &token.kind);
        let kind = kind.unwrap_or(&Kind::Eof);

        if expected_list.contains(kind) {
            Ok(token.unwrap())
        } else {
            let kinds = expected_list
                .iter()
                .map(|k| k.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            panic!("invalid token kind. expected one of {kinds} but got {kind}");
        }
    }
}

impl<'lex> Iterator for Lexer<'lex> {
    type Item = Result<Token<'lex>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(peeked) = self.peeked.take() {
                return Some(peeked);
            }

            let mut chars = self.source.chars().peekable();
            let c = chars.next()?;

            let next = chars.peek().copied();

            break match (c, next) {
                (c, _) if c.is_whitespace() => {
                    self.advance_by(c.len_utf8());
                    continue;
                }
                // ----------------------------------------------------
                // DOUBLE TOKENS
                // ----------------------------------------------------
                ('+', Some('=')) => Some(Ok(self.make_token(Operator::PlusEqual, 2))),
                ('=', Some('=')) => Some(Ok(self.make_token(Operator::EqualEqual, 2))),
                ('=', Some('>')) => Some(Ok(self.make_token(Operator::ThickArrow, 2))),
                ('*', Some('=')) => Some(Ok(self.make_token(Operator::StarEqual, 2))),
                ('/', Some('=')) => Some(Ok(self.make_token(Operator::SlashEqual, 2))),
                ('!', Some('=')) => Some(Ok(self.make_token(Operator::NotEqual, 2))),
                ('<', Some('=')) => Some(Ok(self.make_token(Operator::LessEqual, 2))),
                ('>', Some('=')) => Some(Ok(self.make_token(Operator::GreaterEqual, 2))),
                ('-', Some('=')) => Some(Ok(self.make_token(Operator::MinusEqual, 2))),
                ('&', Some('&')) => Some(Ok(self.make_token(Operator::And, 2))),
                ('|', Some('|')) => Some(Ok(self.make_token(Operator::Or, 2))),
                ('-', Some(c)) if c.is_numeric() => Some(self.lex_numerals()),
                // we ignore a comment until the end of the line
                ('/', Some('/')) => {
                    let eol_location = self.source.find(|c| matches!(c, '\n')).unwrap_or(self.source.len());
                    self.advance_by(eol_location);
                    continue;
                }
                // for multiline comments, we ignore until we find the closing pattern
                ('/', Some('*')) => {
                    // we start past the next 2 characters as we know they are `/*`
                    let mut pos = 2;
                    let mut open_comments = 1;
                    loop {
                        if pos > self.source.len() - 1 {
                            break;
                        }

                        let curr = self.source.chars().nth(pos);
                        let next = self.source.chars().nth(pos + 1);

                        match (curr, next) {
                            (Some('*'), Some('/')) => open_comments -= 1,
                            (Some('/'), Some('*')) => open_comments += 1,
                            _ => (),
                        }

                        pos += 1;

                        if open_comments == 0 {
                            pos += 1;
                            break;
                        }
                    }

                    self.advance_by(pos);

                    continue;
                }
                // ----------------------------------------------------
                // SINGLE TOKENS
                // ----------------------------------------------------
                ('(', _) => Some(Ok(self.make_token(Operator::LeftParen, 1))),
                (')', _) => Some(Ok(self.make_token(Operator::RightParen, 1))),
                ('[', _) => Some(Ok(self.make_token(Operator::LeftBracket, 1))),
                (']', _) => Some(Ok(self.make_token(Operator::RightBracket, 1))),
                ('{', _) => Some(Ok(self.make_token(Operator::LeftBrace, 1))),
                ('}', _) => Some(Ok(self.make_token(Operator::RightBrace, 1))),
                (':', _) => Some(Ok(self.make_token(Operator::Colon, 1))),
                (';', _) => Some(Ok(self.make_token(Operator::SemiColon, 1))),
                (',', _) => Some(Ok(self.make_token(Operator::Comma, 1))),
                ('.', _) => Some(Ok(self.make_token(Operator::Dot, 1))),
                ('+', _) => Some(Ok(self.make_token(Operator::Plus, 1))),
                ('=', _) => Some(Ok(self.make_token(Operator::Equal, 1))),
                ('*', _) => Some(Ok(self.make_token(Operator::Star, 1))),
                ('&', _) => Some(Ok(self.make_token(Operator::Ampersand, 1))),
                ('/', _) => Some(Ok(self.make_token(Operator::Slash, 1))),
                ('!', _) => Some(Ok(self.make_token(Operator::Bang, 1))),
                ('<', _) => Some(Ok(self.make_token(Operator::Less, 1))),
                ('>', _) => Some(Ok(self.make_token(Operator::Greater, 1))),
                ('-', _) => Some(Ok(self.make_token(Operator::Minus, 1))),

                ('a'..='z' | 'A'..='Z' | '_', _) => Some(Ok(self.lex_identifier())),
                ('0'..='9', _) => Some(self.lex_numerals()),
                _ => Some(Ok(Kind::Eof.into_token(self.pos, self.pos))),
            };
        }
    }
}

impl<'lex> Lexer<'lex> {
    fn lex_identifier(&mut self) -> Token<'lex> {
        let next_whitespace = self
            .source
            .find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
            .unwrap_or(self.source.len());

        let identifier = &self.source[..next_whitespace];

        self.make_token(Kind::identifier_from(identifier), next_whitespace)
    }

    fn lex_numerals(&mut self) -> Result<Token<'lex>, Error> {
        let start_byte = self.pos;
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

        let mut postfix = &self.source[..end_of_postfix];
        if postfix.ends_with(';') {
            postfix = postfix.trim_matches(';');
        }

        let is_signed = literal.contains('-');
        let is_float = literal.contains('.');
        let token = match (is_float, is_signed) {
            (false, true) => Primitive::Int {
                value: match literal.parse() {
                    Ok(numeral) => numeral,
                    Err(_) => {
                        return Err(Error::from(self.pos - bytes_eaten..self.pos));
                    }
                },
                size: IntSizes::try_from(postfix).ok(),
            },
            (false, false) => Primitive::UInt {
                value: match literal.parse() {
                    Ok(numeral) => numeral,
                    Err(_) => return Err(Error::from(self.pos - bytes_eaten..self.pos)),
                },
                size: UIntSizes::try_from(postfix).ok(),
            },
            (true, _) => Primitive::Float {
                value: match literal.parse() {
                    Ok(numeral) => numeral,
                    Err(_) => return Err(Error::from(self.pos - bytes_eaten..self.pos)),
                },
                size: FloatSizes::try_from(postfix).ok(),
            },
        };

        let has_size = match &token {
            Primitive::Int { size, .. } => size.is_some(),
            Primitive::UInt { size, .. } => size.is_some(),
            Primitive::Float { size, .. } => size.is_some(),
            _ => unreachable!(),
        };

        if has_size {
            self.source = &self.source[postfix.len()..];
            self.pos += postfix.len();
        }

        Ok(token.into_token(start_byte, self.pos))
    }

    fn advance_by(&mut self, amount: usize) {
        self.source = &self.source[amount..];
        self.pos += amount;
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
            "3.14159265358979323846264338327950288_f32",
            "3.141592653f32;",
        ];
        let source = source.join("\n");

        let mut numerals = vec![];

        for token in make_sut(&source) {
            numerals.push(token.unwrap());
        }

        insta::assert_debug_snapshot!(numerals);
    }

    #[test]
    fn lexing_punctuations() {
        let source = [
            "()", "[]", "{}", ",", ".", "+", "-", "=", "*", "&", "*=", "+=", "-=", "/=", "!", "!=", "==", "<=", ">=",
            "<", ">", "/", ":", ";", "&&", "||",
        ];
        let source = source.join(" ");

        let mut punctuations = vec![];
        for token in make_sut(&source) {
            punctuations.push(token.unwrap());
        }

        insta::assert_debug_snapshot!(punctuations);
    }

    #[test]
    fn lexing_builtin_identifiers() {
        let source = ["var", "const", "match", "if", "else", "fun", "struct", "enum", "return"];

        let source = source.join(" ");

        let mut builtin_identifiers = vec![];
        for token in make_sut(&source) {
            builtin_identifiers.push(token.unwrap());
        }

        insta::assert_debug_snapshot!(builtin_identifiers);
    }

    #[test]
    fn lexing_simple_codeblock() {
        let source = [
            "fun calculate_circumference(diameter: f64) => f64 {",
            "   const pi = 3.14159265358979323846264338327950288_f32;",
            "   const radius = diameter / 2.0;",
            "   const circumference = 2.0 * pi * radius;",
            "",
            "   circumference",
            "}",
        ];

        let source = source.join("\n");

        let mut calculate_circumference_function = vec![];
        for token in make_sut(&source) {
            calculate_circumference_function.push(token.unwrap());
        }

        insta::assert_debug_snapshot!(calculate_circumference_function);
    }
}
