# wowcaml

<p align="center">
<img height="250px" src="assets/wowcaml.png"/>
</p>
<br>

[![CI](https://img.shields.io/github/workflow/status/femtomc/mincaml/CI?style=for-the-badge)](https://github.com/femtomc/mincaml/actions?query=workflow%3ACI)

This is a fork of [this Rust implementation of `mincaml`](https://github.com/osa1/mincaml), updated to recent versions of [cranelift](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift).

It appears the original author has abandoned this project (and is on to better things!), so I'm going to pick it up and try and do some fun stuff with it as I play around with functional language compilation.

- [ ] Eventually, I'll probably add an LLVM backend through [inkwell](https://github.com/TheDan64/inkwell).
- [ ] Maybe, just maybe -- I'll add a GC!
- [ ] [What if we had a _compile-time_ GC using uniqueness/linear types?](https://github.com/granule-project/granule)
- [ ] [Wow, 1ML looks crazy!](https://people.mpi-sws.org/~rossberg/1ml/)
- [ ] [But `System F` seems to be a concise language core...](https://www.youtube.com/watch?v=u9bY0Bc_lXw)
- [ ] [Also, Koka seems to be built on top of `System F`...](https://www.youtube.com/watch?v=6OFhD_mHtKA)
