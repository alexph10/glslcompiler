use clap::{Parser, ValueEnum};
use GLSLCompiler;
use std::error::Error;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process;

/// Command-line interface for the pixdsl compiler
#[derive(Parser)]
#[command(name = "glslcompiler")]
#[command(about = "A domain-specific language compiler for procedural fragment shaders")]
#[command(version = "0.1.0")]
#[command(author = "pixdsl team")]
struct Args {
    /// Input .dsl file to compile
    #[arg(long, short, value_name = "FILE")]
    input: PathBuf,

    /// Output file path (defaults to stdout if not specified)
    #[arg(long, short, value_name = "FILE")]
    output: Option<PathBuf>,

    /// Target GLSL profile for compilation
    #[arg(long, short, value_enum, default_value = "es300")]
    profile: Profile,
}

/// Available GLSL compilation targets
#[derive(Debug, Clone, Copy, ValueEnum)]
enum Profile {
    /// GLSL ES 3.00 for WebGL2 and mobile devices
    #[value(name = "es300")]
    Es300,
    /// GLSL 3.30 core profile for desktop OpenGL
    #[value(name = "gl330")]
    Gl330,
}

impl From<Profile> for GLSLCompiler::Profile {
    fn from(profile: Profile) -> Self {
        match profile {
            Profile::Es300 => GLSLCompiler::Profile::Es300,
            Profile::Gl330 => GLSLCompiler::Profile::Gl330,
        }
    }
}

/// Main entry point with comprehensive error handling
fn main() {
    if let Err(exit_code) = run() {
        process::exit(exit_code);
    }
}

/// Run the compiler with proper error handling and reporting
fn run() -> Result<(), i32> {
    let args = Args::parse();

    // Read and validate input file
    let source = read_input_file(&args.input)?;
    
    // Create compiler instance
    let compiler = GLSLCompiler::Compiler::new(args.profile.into());
    
    // Compile source to GLSL
    let glsl_output = match compiler.compile(&source) {
        Ok(output) => output,
        Err(err) => {
            eprintln!("Compilation failed: {}", err);
            
            // Print additional context for parser/lexer errors
            if let Some(source_err) = err.source() {
                eprintln!("  Caused by: {}", source_err);
            }
            
            return Err(1);
        }
    };
    
    // Write output to file or stdout
    write_output(&glsl_output, args.output.as_deref())?;
    
    Ok(())
}

/// Read input file with proper error handling
fn read_input_file(path: &PathBuf) -> Result<String, i32> {
    match fs::read_to_string(path) {
        Ok(content) => Ok(content),
        Err(err) => {
            eprintln!("Failed to read input file '{}': {}", path.display(), err);
            Err(2)
        }
    }
}

/// Write output to file or stdout with error handling
fn write_output(content: &str, output_path: Option<&Path>) -> Result<(), i32> {
    match output_path {
        Some(path) => write_to_file(content, path),
        None => write_to_stdout(content),
    }
}

/// Write content to a specific file
fn write_to_file(content: &str, path: &Path) -> Result<(), i32> {
    match fs::write(path, content) {
        Ok(()) => {
            eprintln!("Successfully wrote output to '{}'", path.display());
            Ok(())
        }
        Err(err) => {
            eprintln!("Failed to write output file '{}': {}", path.display(), err);
            Err(3)
        }
    }
}

/// Write content to stdout
fn write_to_stdout(content: &str) -> Result<(), i32> {
    match io::stdout().write_all(content.as_bytes()) {
        Ok(()) => {
            if let Err(err) = io::stdout().flush() {
                eprintln!("Failed to flush stdout: {}", err);
                return Err(4);
            }
            Ok(())
        }
        Err(err) => {
            eprintln!("Failed to write to stdout: {}", err);
            Err(4)
        }
    }
}
