# wowcaml

<p align="center">
<img height="250px" src="assets/wowcaml.png"/>
</p>
<br>

[![CI](https://img.shields.io/github/workflow/status/femtomc/wowcaml/CI?style=for-the-badge)](https://github.com/femtomc/wowcaml/actions?query=workflow%3ACI)

This is a fork of [this Rust implementation of `mincaml`](https://github.com/osa1/mincaml) - a compiler for a call-by-value ML-like language, updated to recent versions of [cranelift](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift).

It appears the original author has abandoned this project (and is on to better things!), so I'm going to pick it up and try and do some fun stuff with it as I play around with functional language compilation.

- [ ] Implement an LLVM backend.
- [ ] [I should probably make it properly polymorphic.](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)
- [ ] Maybe, just maybe -- I'll add a GC!
- [ ] [What if we had a _compile-time_ GC using uniqueness/linear types?](https://github.com/granule-project/granule)
- [ ] [Wow, 1ML looks crazy!](https://people.mpi-sws.org/~rossberg/1ml/)
- [ ] [But `System F` seems to be a concise language core...](https://www.youtube.com/watch?v=u9bY0Bc_lXw)
- [ ] [Even Simon admits that a strict Haskell might have been good...](https://www.youtube.com/watch?v=re96UgMk6GQ)
- [ ] [Also, Koka seems to be built on top of `System F`...](https://www.youtube.com/watch?v=6OFhD_mHtKA)

## Usage

Build the compiler -- this will place `wmlcc` into `target`.

```
cargo build
```

After building, you should be able to use the compiler on `ubuntu-latest`, even `apple-darwin`.

Assuming you have a reference `ocamlc`, you can run all the test programs with

```
./target/debug/test
```
