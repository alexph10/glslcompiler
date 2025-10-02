use clap::{Parser, ValueEnum};
use GLSLCompiler;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "glslcompiler")]
#[command(about = "A compiler for the GLSL shader language")]
#[command(version = "0.1.0")]
struct Args {
    /// Input .dsl file to compile
    #[arg(long, short)]
    input: PathBuf,

    /// Output file (defaults to stdout)
    #[arg(long, short)]
    output: Option<PathBuf>,

    /// Target GLSL profile
    #[arg(long, short)]
    profile: Profile,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum Profile {
    /// GLSL ES 3.00 for WebGL2
    #[value(name = "es300")]
    Es300,
    /// GLSL 3.30 for desktop OpenGL
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

fn main() {
    let args = Args::parse();

    // Read input file
    let source = match fs::read_to_string(&args.input) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading input file '{}': {}", args.input.display(), err);
            std::process::exit(1);
        }
    };

    // Compile the DSL to GLSL
    let glsl_output = match GLSLCompiler::compile(&source, args.profile.into()) {
        Ok(glsl) => glsl,
        Err(err) => {
            eprintln!("Compilation error: {}", err);
            std::process::exit(1);
        }
    };

    // Write output
    match args.output {
        Some(output_path) => {
            if let Err(err) = fs::write(&output_path, &glsl_output) {
                eprintln!("Error writing output file '{}': {}", output_path.display(), err);
                std::process::exit(1);
            }
            println!("Compiled successfully to '{}'", output_path.display());
        }
        None => {
            // Write to stdout
            print!("{}", glsl_output);
            io::stdout().flush().unwrap();
        }
    }
}
