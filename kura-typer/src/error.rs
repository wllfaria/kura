use std::sync::Arc;

use kura_lexer::token::Location;
use miette::{Diagnostic, LabeledSpan, NamedSource, SourceSpan};

#[derive(Debug, Clone, thiserror::Error)]
#[error("Type error")]
pub struct Error {
    label_span: SourceSpan,
    label_message: String,
    help: Option<String>,
    src: NamedSource<Arc<String>>,
}

impl Error {
    pub fn new(location: Location, label_message: String, help: Option<String>, src: NamedSource<Arc<String>>) -> Self {
        Self {
            src,
            help,
            label_message,
            label_span: (location.start_byte..location.end_byte).into(),
        }
    }
}

impl Diagnostic for Error {
    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        Some(&self.src)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        Some(Box::new(std::iter::once(LabeledSpan::new_with_span(
            Some(self.label_message.clone()),
            self.label_span,
        ))))
    }

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.help.as_ref().map(|s| Box::new(s) as Box<dyn std::fmt::Display>)
    }
}
