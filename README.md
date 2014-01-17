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
- Malleable syntax with Common Lisp-esque macros
- Dead-simple parallelism with special lists called "sequences"
- Orthogonal core syntax and semantics for your performance and my sanity

1. Status
---

**SUPER DUPER INCOMPLETE. LIKE, THE PARSER ISN'T EVEN DONE.** Right now what
builds is a nice, lame interpreter that spits out the parse of what you typed.
Not even all of the core syntax is enabled.

However, the code is somewhat useful right now as an example of how to use
[parsec][parsec] with free monads being used as a [mu combinator][mu]. That
means it'll be real easy to perform type checking with comonads [as per this
article][comonads].

### 1.1 Roadmap

1. Take a file as input, not a REPL
2. Basic type checking
3. Add unique types to the kind system
4. Algebraic data types (as closures)
5. We'll see?

2. Why?
---

Because I want Haskell's type system, lisp's homiconicity, sequenceL's
normalize-transpose operation, and Rust's memory management.

Also because I have never made a programming language before.

3. Some kind of example?
---

The goal is to provide a practical median between a minimal, orthogonal core
language and something useful in real world scenarios.

Right now the core syntax is a slightly modified lambda calculus wherein all
functions are unary; *however* all functions take as their single parameter a
list (much like Perl, actually) which is deconstructed to pass arguments.

### 3.1 Okay examples now!

Here's what happens when I evaluate the Y combinator (in the core syntax):

    ;; Y combinator
    (fn (f)
      ((fn (y)
         (f (y y)))
       (fn (y)
         (f (y y)))))

    ;; result

    Mu (
      ALambda Anonymous Mu (
        AList [
          Mu ( ASymbol "f" )
        ]
      )
      Mu (
        AApply
          Mu (
            ALambda Anonymous Mu (
              AList [
                Mu ( ASymbol "y" )
              ]
            )
            Mu (
              AApply
                Mu ( ASymbol "f" )
                Mu (
                  AList [
                    Mu (
                      AApply
                        Mu ( ASymbol "y" )
                        Mu (
                          AList [
                            Mu ( ASymbol "y" )
                          ]
                        )
                    )
                  ]
                )
            )
          )
          Mu (
            AList [
              Mu (
                ALambda Anonymous Mu (
                  AList [
                    Mu ( ASymbol "y" )
                  ]
                )
                Mu (
                  AApply
                    Mu ( ASymbol "f" )
                    Mu (
                      AList [
                        Mu (
                          AApply
                            Mu ( ASymbol "y" )
                            Mu (
                              AList [
                                Mu ( ASymbol "y" )
                              ]
                            )
                        )
                      ]
                    )
                )
              )
            ]
          )
      )
    )


`Mu` here is, incidentally, the Mu combinator, a type-level equivalent to the Y combinator.

Here are some examples of lists and the quote, quasiquote, and unquote operators:
    ready> '(a (b c) d)
    Mu ( AList [Mu ( ASymbol "quote" ) ,Mu ( AList [Mu ( ASymbol "a" ) ,Mu ( AList [Mu ( ASymbol "b" ) ,Mu ( ASymbol "c" ) ] ) ,Mu ( ASymbol "d" ) ] ) ] )

    ready> `(a (b c) d)
    Mu ( AList [Mu ( ASymbol "quasi" ) ,Mu ( AList [Mu ( ASymbol "a" ) ,Mu ( AList [Mu ( ASymbol "b" ) ,Mu ( ASymbol "c" ) ] ) ,Mu ( ASymbol "d" ) ] ) ] )

    ready> `(a ,(b c) d)
    Mu ( AList [Mu ( ASymbol "quasi" ) ,Mu ( AList [Mu ( ASymbol "a" ) ,Mu ( AList [Mu ( ASymbol "comma" ) ,Mu ( AApply Mu ( ASymbol "b" )  Mu ( AList [Mu ( ASymbol "c" ) ] )  ) ] ) ,Mu ( ASymbol "d" ) ] ) ] ) 

Also supported are let bindings, which boil down to lambda functions:

    ;; input

    (let ((x (f))
          (y 1))
      (x y))

    ((fn (x y)
       (x y))
     (f) 1)

    ;; output (for both)

    Mu (
      AApply
        Mu (
          ALambda Anonymous Mu (
            AList [
              Mu ( ASymbol "x" )
              Mu ( ASymbol "y" )
            ]
          )
          Mu (
            AApply
              Mu ( ASymbol "x" )
              Mu (
                AList [
                  Mu ( ASymbol "y" )
                ]
              )
          )
        )
        Mu (
          AList [
            Mu (
              AApply
                Mu ( ASymbol "f" )
                Mu (
                  AList []
                )
            )
            Mu ( AInteger 1 )
          ]
        )
    )

### 3.2 More examples!

More information can be found in the `eg` directory. Those files are valid
input to the compiler as it stands (however incomplete).

4. How to build
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

5. Questions / comments / hate mail
---

Use the Issues feature of GitHub.

[parsec]: http://hackage.haskell.org/package/parsec

[mu]:
http://debasishg.blogspot.com/2012/01/learning-type-level-fixpoint-combinator.html

[comonads]: http://brianmckenna.org/blog/type_annotation_cofree

[haskellplatform]: http://haskell.org/platform
