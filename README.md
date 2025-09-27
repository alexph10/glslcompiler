## Why this exists
Shader authoring can be opaque for newcomers. `pixdsl` separates **artist-level syntax** from **backend details** (GLSL ES 300 vs GLSL 330) and provides a clean path to add features methodically (lexer → parser → types → codegen).

## Features (MVP scope)
- Simple expression grammar (functions, let-bindings, calls)
- Types: `float`, `vec2`, `vec3`, `vec4`, simple structs (later)
- Built-ins: `sin`, `cos`, `mix`, `clamp`, `smoothstep`, etc.
- Noise: bundled value noise (upgradeable to simplex)
- Backends: GLSL ES 300 and GLSL 330 wrappers

## Quick start
```bash
# Build & run (prints GLSL)
cargo run -- --input examples/noise.dsl --profile es300

# Write to a file
cargo run -- --input examples/noise.dsl --output out.frag --profile gl330
