//! This module contains
//! a [cranelift](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift)-based code generation backend.
//!
//! It exports two "toplevel" entry points for code generation:
//!
//! 1. `codegen` -- which provides ahead-of-time object code generation
//! via `cranelift_object`.
//!
//! 2. `codegen_jit` -- which provides just-in-time code generation via `cranelift_jit`.
//!
//! The JIT functionality is used by the REPL.

pub mod cranelift;
pub use self::cranelift::*;
