# pistachio

<p align="center">
<img height="200px" src="pistachio.png"/>
</p>
<br>

I want to learn about how theorem provers and dependently typed languages are implemented. This is a little repo for experimentation.

- [elaboration-zoo](https://github.com/AndrasKovacs/elaboration-zoo)
- [pi-forall](https://github.com/sweirich/pi-forall)
- [tartlet](https://davidchristiansen.dk/tutorials/nbe/)
- [Victor Maia's post on a small theorem prover](https://medium.com/@maiavictor/towards-a-simple-theorem-prover-5005a1e66a6f)

**Things I want to learn about**

- [ ] Why [normalization by evaluation](https://en.wikipedia.org/wiki/Normalisation_by_evaluation)?

  It seems the reason for this is efficiency, from (Christiansen, _Checking Dependent Types with Normalization by Evaluation_):

  > One way to find the normal form of an expression would be to repeatedly traverse it, performing all possible β-reductions. However, this is extremely inefficient—first, a redex must be located, and then a new expression constructed by applying the β rule. Then, the context around the former redex must be reconstructed to point at the newly-constructed expression. Doing this for each and every redex is not particularly efficient, and the resulting code is typically not pleasant to read.

- [ ] [Does exposing interfaces to the elaborator enable typed metaprogramming?](https://docs.idris-lang.org/en/latest/elaboratorReflection/elabReflection.html)

- [ ] [What are the standard compilation techniques for a dependently typed language? Do they differ from normal functional techniques?](https://github.com/femtomc/wowcaml)

- [ ] How do you compile elaborator strategies? [Can you stage the interpreter?](https://dl.acm.org/doi/10.1145/3428232)

- [ ] How does a module system interact with elaboration?

- [ ] How do computational effects interact with elaboration, and proofs?

- [ ] From [`peridot`](https://github.com/eashanhatti/peridot), what does a dependently typed object language gain us?

  `peridot` is based on _two-level type theory_ - something which I ultimately think I'll have to consider. The meta-language of `peridot` is a specially designed logic language for working with terms of the object-language. Crucially (and something I've recently realized), if the object language is dependently typed, we gain the ability to make statements about types and term equality! That means we can reason about the semantic correctness of optimizers written in the meta-language which take term to term (the meta-language is resolved completely at compile time).

  Ultimately, `peridot` has uncovered this very deep and useful idea using dependent types. From my perspective, I need to investigate the meta-language - determine if it is sufficient to express the types of transformations I wish to express.

  Another paper worth exploring here: [Meta-F\*: Proof Automation with SMT, Tactics, and Metaprograms](https://arxiv.org/pdf/1803.06547.pdf)

## Why is this?

When it comes to dependent types, my main question is about runtime performance. Do dependent types provide a substrate for communicating with the compiler in a direct way? More generally, is it possible to construct a language system where metaprograms (optimizers, program transformers, etc) are also specified and checked?

If yes, it's possible that a language system like this could provide a useful basis for modern techniques in numerical method:s (like machine learning). There are plenty of new language papers on automatic differentation, including implementations via delimited continuations, and source-to-source program transformations, etc. Instead of building a transformation like this into the compiler, what's the right language platform to allow a library of metaprograms to implement fully specified AD.

Probabilistic programming is another area which motivates me. Large scale (repeated) inference often requires re-evaluating functions with small modifications to inputs. Static techniques have been developed for this use case -- and expressed by program transformers. What sort of language allows me to write these transformers in user-space?

The main downside of dependent types seems to be ergonomics: the implementation is complicated. Proving things is complicated. Exposing metaprogramming to programmers via elaboration extension also appears to be a complicated thing.

## Prior art?

There's tons of related ideas here. Probably `F*` is the language which is "closest" to what I want. However, I don't understand how `F*` is implemented! At least for me, if I can't see how to implement it -- I don't understand it. So I want a small language which supports some of the features of `F*`:

1. Functional and strict.
2. [Functional, but in place](https://www.microsoft.com/en-us/research/uploads/prod/2020/11/perceus-tr-v1.pdf)
3. Metaprograms, via elaborator reflection.
4. Effectful.

Ergonomics aside -- we can focus on (1, 3, 4) as "semantic" features of the language. (2) is how you make a language like this go fast. JIT compiling (3) is how you make metaprograms go fast.

The other system which is directly related to my ideas is [`peridot`](https://github.com/eashanhatti/peridot). The author of this language wants the user to be able to write the compiler in user-space! This is very close to what I want to achieve. Similar to `F*`, I don't understand `peridot`! Hence the experiments.
