psilo
=====

a parallel, streaming, iterative list operation language for writing interesting
programs. [View it on GitHub.](https://github.com/gatlin/psilo)

&copy; 2014 [Gatlin Johnson](http://niltag.net) <gatlin@niltag.net>

What is psilo?
===

psilo, like any lisp, will be a **lis**t **p**rocessor. However psilo will
take advantage of a strong type system, linear types, and aggressive
optimization techniques such as shortcut fusion. The goal is to make writing
fast, resource-aware, parallelized stream processing programs as simple as
possible.

It is also nowhere close to being finished; at the moment this is purely
exploratory.

Technical Features (planned):

- No run-time garbage collection necessary owing to uniqueness types
- Static typing for compile-time verification and optimization
- Malleable syntax with macros
- Dead-simple parallelism via pipelines and special list types
- Monadic continuations and iteratee composition made dead simple
- Orthogonal core syntax and semantics for your performance and my sanity

Philosophy:

- All programming is manipulating languages.
- Types define grammars; functions define parsers.
- The earlier a question may be answered, the better.
- If the computer can do it, it should.

Status
===

Psilo is still being designed. I have written a really simple evaluator for
prototyping and experimenting with the language which is actively being
developed.

While not production quality, the simple interpreter might be of educational
value.

Here is some code the interpreter runs right now:

    ; Basic untyped list definition and some functions
    (= cons (x y)
      (\ (c n) (c x y)))

    (= nil ()
      (\ (c n) (n)))

    (= car (xs)
      (xs (\ (y ys) y)))

    (= cdr (xs)
      (xs (\ (y ys) ys)))

    (= foldr (xs a b)
      (xs (\ (y ys) (a y (foldr ys a b)))
          nil))

    (= sum (xs)
      (foldr xs (\ (x y) (+ x y)) 0))

    ; this one is faulty
    (= map (f xs)
      (foldr xs
        (\ (y ys) (cons (f y) ys))
        nil))

    (= length (xs)
      (foldr xs (\ (y ys) (+ 1 ys)) 0))

    (= square (x) (* x x))

    (let
      ((lst1 (cons 3 (cons 2 (cons 1 (nil))))))
      (print (car (cdr (map square lst1)))))

How to build
===

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

Questions / comments / hate mail
===

Use the Issues feature of GitHub or email me: <gatlin@niltag.net>.

[parsec]: http://hackage.haskell.org/package/parsec

[mu]:
http://debasishg.blogspot.com/2012/01/learning-type-level-fixpoint-combinator.html

[comonads]: http://brianmckenna.org/blog/type_annotation_cofree

[haskellplatform]: http://haskell.org/platform
