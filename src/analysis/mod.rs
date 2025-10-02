use crate::span::Span;
use thiserror::Error;

/// Errors that can occur during semantic analysis
#[derive(Debug, Error, Clone, PartialEq)]
pub enum TypeError {
    #[error("Type mismatch: expected {expected}, found {found} at {span:?}")]
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },
    
    #[error("Unknown identifier 
\
name
\' at {span:?}")]
    UnknownIdentifier { name: String, span: Span },
    
    #[error("Function 
\
name
\' called with wrong number of arguments: expected {expected}, found {found} at {span:?}")]
    ArityMismatch {
        name: String,
        expected: usize,
        found: usize,
        span: Span,
    },
    
    #[error("Unknown function 
\
name
\' at {span:?}")]
    UnknownFunction { name: String, span: Span },
}

// TODO: Add type system and type checker implementation
// This is a stub for now to satisfy the error type imports
