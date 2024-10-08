use std::fmt;
use std::ops::{Range, RangeBounds};

use super::kind::Kind;

pub trait IntoToken<'tok> {
    fn into_token(self, start_byte: usize, end_byte: usize) -> Token<'tok>;
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Location {
    pub start_byte: usize,
    pub end_byte: usize,
}

impl Location {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start_byte: start,
            end_byte: end,
        }
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "start byte: {}, end byte {}", self.start_byte, self.end_byte)
    }
}

impl From<(usize, usize)> for Location {
    fn from((start_byte, end_byte): (usize, usize)) -> Self {
        Self { start_byte, end_byte }
    }
}

impl From<Range<usize>> for Location {
    fn from(range: Range<usize>) -> Self {
        let start = match range.start_bound() {
            std::ops::Bound::Included(start) => *start,
            std::ops::Bound::Excluded(start) => *start,
            std::ops::Bound::Unbounded => {
                panic!("can only construct a location from bounded ranges")
            }
        };

        let end = match range.end_bound() {
            std::ops::Bound::Included(end) => *end,
            std::ops::Bound::Excluded(end) => *end,
            std::ops::Bound::Unbounded => {
                panic!("can only construct a location from bounded ranges")
            }
        };

        Location {
            start_byte: start,
            end_byte: end,
        }
    }
}

#[derive(Debug)]
pub struct Token<'tok> {
    pub kind: Kind<'tok>,
    pub location: Location,
}

impl<'tok> Token<'tok> {
    pub fn new(kind: Kind<'tok>, location: Location) -> Self {
        Self { kind, location }
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}
