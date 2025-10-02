use thiserror::Error;

/// Errors that can occur during code generation
#[derive(Debug, Error, Clone, PartialEq)]
pub enum CodegenError {
    #[error("Unsupported feature '{feature}' for profile {profile}")]
    UnsupportedFeature { feature: String, profile: String },
    
    #[error("Internal codegen error: {message}")]
    Internal { message: String },
}

// TODO: Add codegen backends and traits
// This is a stub for now to satisfy the error type imports
