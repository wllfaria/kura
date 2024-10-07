use crate::token::Location;

#[derive(Debug, Default)]
pub struct Error {
    location: Location,
    message: String,
}

impl Error {
    pub fn with_message(&mut self, message: String) {
        self.message = message;
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} @ {}", self.message, self.location)
    }
}

impl std::error::Error for Error {}

impl From<std::ops::Range<usize>> for Error {
    fn from(value: std::ops::Range<usize>) -> Self {
        Self {
            location: value.into(),
            message: Default::default(),
        }
    }
}

impl From<Location> for Error {
    fn from(location: Location) -> Self {
        Self {
            location,
            message: Default::default(),
        }
    }
}
