//! pixdsl - A domain-specific language for procedural fragment shaders
//! 
//! This crate provides a compiler that transpiles pixdsl source code to GLSL
//! fragment shaders for different target profiles (WebGL2 ES 3.00, Desktop GL 3.30).

pub mod lexer;
pub mod parser;
pub mod analysis;
pub mod builtin;
pub mod codegen;
pub mod span;

use std::fmt;
use thiserror::Error;

/// The main compilation profiles supported by pixdsl
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Profile {
    /// GLSL ES 3.00 for WebGL2 and mobile devices
    Es300,
    /// GLSL 3.30 core profile for desktop OpenGL
    Gl330,
}

impl fmt::Display for Profile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Profile::Es300 => write!(f, "es300"),
            Profile::Gl330 => write!(f, "gl330"),
        }
    }
}

/// Comprehensive error type for all compilation stages
#[derive(Debug, Error)]
pub enum CompilerError {
    #[error("Lexical error: {0}")]
    Lex(#[from] lexer::LexError),
    
    #[error("Parse error: {0}")]
    Parse(#[from] parser::ParseError),
    
    #[error("Type error: {0}")]
    Type(#[from] analysis::TypeError),
    
    #[error("Code generation error: {0}")]
    Codegen(#[from] codegen::CodegenError),
    
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
}

/// Compilation result
pub type Result<T> = std::result::Result<T, CompilerError>;

/// Main compiler interface
pub struct Compiler {
    profile: Profile,
}

impl Compiler {
    /// Create a new compiler for the specified target profile
    pub fn new(profile: Profile) -> Self {
        Self { profile }
    }
    
    /// Compile pixdsl source code to GLSL
    pub fn compile(&self, source: &str) -> Result<String> {
        // Step 1: Parse the source code into an AST
        let mut parser = parser::Parser::new(source)?;
        let ast = parser.parse()?;
        
        // Step 2: Generate GLSL from the AST
        // TODO: Replace this stub with real code generation
        self.generate_glsl_stub(&ast)
    }
    
    /// Temporary stub for GLSL generation - will be replaced with real codegen
    fn generate_glsl_stub(&self, _ast: &parser::Program) -> Result<String> {
        let version_directive = match self.profile {
            Profile::Es300 => "#version 300 es",
            Profile::Gl330 => "#version 330 core",
        };

        let precision_qualifiers = match self.profile {
            Profile::Es300 => "precision mediump float;\n",
            Profile::Gl330 => "", // Desktop doesn't need precision qualifiers
        };

        let main_function = match self.profile {
            Profile::Es300 => {
                "in vec2 v_texcoord;\nout vec4 fragColor;\n\nvoid main() {\n    fragColor = vec4(1.0, 0.0, 0.0, 1.0); // Red\n}"
            }
            Profile::Gl330 => {
                "in vec2 v_texcoord;\nout vec4 fragColor;\n\nvoid main() {\n    fragColor = vec4(1.0, 0.0, 0.0, 1.0); // Red\n}"
            }
        };

        Ok(format!(
            "{}\n{}\n{}",
            version_directive, precision_qualifiers, main_function
        ))
    }
}

/// Convenience function for one-shot compilation
/// 
/// # Example
/// ```
/// use glslcompiler::{compile, Profile};
/// 
/// let dsl_source = "vec4(1.0, 0.0, 0.0, 1.0)";
/// let glsl = compile(dsl_source, Profile::Es300)?;
/// println!("{}", glsl);
/// # Ok::<(), glslcompiler::CompilerError>(())
/// ```
pub fn compile(source: &str, profile: Profile) -> Result<String> {
    let compiler = Compiler::new(profile);
    compiler.compile(source)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_basic_es300() {
        let result = compile("vec4(1.0, 0.0, 0.0, 1.0)", Profile::Es300);
        assert!(result.is_ok());
        let glsl = result.unwrap();
        assert!(glsl.contains("#version 300 es"));
        assert!(glsl.contains("precision mediump float"));
    }

    #[test]
    fn test_compile_basic_gl330() {
        let result = compile("vec4(1.0, 0.0, 0.0, 1.0)", Profile::Gl330);
        assert!(result.is_ok());
        let glsl = result.unwrap();
        assert!(glsl.contains("#version 330 core"));
        assert!(!glsl.contains("precision")); // Desktop doesn't need precision
    }

    #[test]
    fn test_compiler_reuse() {
        let compiler = Compiler::new(Profile::Es300);
        let result1 = compiler.compile("vec4(1.0, 0.0, 0.0, 1.0)");
        let result2 = compiler.compile("vec4(0.0, 1.0, 0.0, 1.0)");
        
        assert!(result1.is_ok());
        assert!(result2.is_ok());
    }
}