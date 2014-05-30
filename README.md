psilo
=====

a parallel, safe, iterative list operation language for systems programming

(c) 2014 [Gatlin Johnson](http://niltag.net) <gatlin@niltag.net>

0. What is psilo?
---

psilo will be a functional systems programming language. Its major goals /
features:

- No run-time garbage collection necessary owing to uniqueness types
- Static typing for compile-time verification and optimization
- Malleable syntax with fexpr-esque macros
- Dead-simple parallelism with special lists called vectors
- Orthogonal core syntax and semantics for your performance and my sanity

### Status

**29 May 2014** A nascent evaluator is now committed. By no means is it
complete, but I can successfully run computations in a rough subset of the
language, so that's something!

The parser is not done but I am now on much better footing than I was before so
I can continue with it as planned.

Also the source code is now Literate Haskell as I want the psilo compiler to
also be a human-language guide to the language's structure and implementation.

1. Synopsis
---

The grammar is a work in progress. At the moment, psilo code looks like this:

    (let ((square (\ (x) (* x x)))
          (add1   (\ (x) (+ 1 x))))
      (add1 (square 5)))

    ; => 26

    (let ((square (\ (x) (* x x)))
          (v      [ 1 2 3 4 5 ] ))
      (square v))

    ; => [ 1 4 9 16 25 ]

3. How to build
---

You need the Glasgow Haskell Compiler and a number of libraries; I suggest
starting off with [the Haskell platform][haskellplatform].

Clone the repository:

    git clone https://github.com/gatlin/psilo

Set up a cabal sandbox:

    cabal sandbox init
    cabal configure
    cabal install --only-dependencies

Then make with:

    make

And return to the Edenic, pre-build post-checkout status of the code with

    make clean

4. Questions / comments / hate mail
---

Use the Issues feature of GitHub.

[parsec]: http://hackage.haskell.org/package/parsec

[mu]:
http://debasishg.blogspot.com/2012/01/learning-type-level-fixpoint-combinator.html

[comonads]: http://brianmckenna.org/blog/type_annotation_cofree

[haskellplatform]: http://haskell.org/platform
