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
- Dead-simple parallelism with special lists called vectors
- Orthogonal core syntax and semantics for your performance and my sanity

### Status

The parser isn't even completed, the typechecker is nonexistent, and I'm not
fully done with how the grammar should look. This README is essentially psilo
at the moment.

1. Synopsis
---

### Basics

psilo is strongly typed, implementing a type system akin to Haskell's:

    (:: square ((Numeric n) :=> n :-> n))
    (fn square (&x) (* x x))

But type annotations are almost always optional:

    (fn add1 (n) (+ 1 n))

Closures are not only possible but encouraged:

    (fn read-some-pipe (config)
      (let ((pipe (make-pipe config)))
        (fn ()
          (read-pipe pipe))))

If you find yourself writing code like this:

    (fn ex1 ()
      (let ((x (foo)))
        (let ((y (bar x)))
          (baz y x))))

you can write it like so:

    (fn ex1 ()
      (do (= x (foo))
          (= y (bar x))
          (baz y x)))

Additionally, for readibility you can make a function infix using the
following convention:

    (fn ex1 ()
      (do (x := (foo))
          (y := (bar x))
          (baz y x)))

### Linear types

Under special circumstances, you can also mutate values:

    (fn ex2 ()
      (do (x := 5)
          (x := (x :+ 1))
          x))

If value is used *linearly*, it may be mutated. A linear value must be used
exactly once in any scope before it is reassigned or returned, or
it must be explicitly freed.

    (fn ex3-good ()
      (do (x := 5)
          (x := (+ 1 x))
          (y := (square x))
          y))

    ; bad: x is mutated, but used twice
    (fn ex4-bad ()
      (do (x := 5)
          (x := (x :+ 1))
          (y := (square x))
          (z := (1 :+ (square x)))
          (y :* z)))

However, you can *reference* a variable in a function argument list in order to
get an immutable copy of a value that you may share to you heart's content ...
within that function.

    ; x is linear because it is eventually reassigned, but it can be used
    ; twice inside "square" because of the reference
    (fn square (&x) (* x x))
    (fn ex5 ()
      (do (x := 5)
          (x := (square x))
          x))

Since mutable values are linear by default anyway, you can be certain when a
reference is made that there will be no others at the same time.

### Conditionals

Psilo doesn't actually have an if statement in the language, but the standard
library comes with short-circuiting `and` and `or` functions, and I included
this anyway:

    (:: if (Boolean :-> a :-> a :-> a))
    (fn if (condition then else)
      (or (and condition then) else))

Otherwise psilo has one native conditional form, the case:

    (fn ex6 (val)
      (? val
        ((0 ("Condition 0"))
         (1 ("Condition 1"))
         (_ ("Unknown condition")))))

You must take care to cover all cases, though the `_` symbol can help with
that.

### Pointers

Psilo has pointers, too:

    ; create a pointer to an integer, update the value at the pointer
    (fn ex7 ()
      (do (x := (make 5))
          (*x := (1 :+ *x))
          *x))

All pointers are linear, no exceptions.

### Lists and Vectors

Psilo has two native aggregate data structures: lists and vectors.

Lists are heterogeneously typed ordered multi-sets. Lists are fixed in length.

You create a list by quoting an expression:

    '(1 2 3)

Lists are used to write functions which accept or return multiple values:

    (:: check-status (server-name)
      (? (is-running server-name)
        (True '(1 "Server is running"))
        (False '(0 "Server is not running"))))

Different kinds of lists are also important in the macro facilities (TODO).

Vectors are different. They are homogeneously typed ordered multi-sets. Vectors
are of arbitrary length and may be extended. Vectors have a special property
called normalize-transpose:

    (fn square (&x) (* x x))
    (square [1 2 3])

    ; => [1 4 9]

Or a more complicated example:

    (:: ex8 ((Numeric n) :=> [n]))
    (fn ex8 ()
      (do (v1 := [10 20 30])
          (v2 := [1 2])
          (v3 := (v1 :+ v2))
          v3))

    ; => [11 12 21 22 31 32]

When you give a vector of values of type `a` (denoted `[a]`) to a function
which expects a scalar `a`, the operation is *normalized* and then *transposed*
such that one gets a predictably shaped vector outcome.

### Algebraic Data Types

You can create algebraic data types like so:

    (adt Stream (a)
      ((Nil)
       (Cons a (Stream a))))

And you can write pattern-matching functions for them:

    (:: stream-length ((Stream a) :-> Int))
    (fn stream-length ((Nil)) 0)
    (fn stream-length ((Cons h t))
      (1 :+ (stream-length t)))

    (:: stream-to-vector ((Stream a) :-> [a]))
    (fn stream-to-vector ((Nil)) [])
    (fn stream-to-vector ((Cons h t))
      ([h] :++ (stream-to-vector t)))

### Delimited continuations

By default, computations execute sequentially. However, you can alter your
program's flow-control semantics and strengthen type safety with delimited
continuations.

The following example demonstrates an IO continuation that is almost the same
as the default but which mandates the usage of specific functions:

    (cont SimpleIO
      ((:: say (String :-> (SimpleIO ())))
       (fn say (s)
         (let ((_ (c-printf))) (call/cc k))))

      ((:: read (SimpleIO String))
       (fn read ()
         (do (in := (c-scanf))
             (call/cc in)))))

    ; A function which can only be used within our continuation
    (:: prompt (String :-> (SimpleIO String)))
    (fn prompt (s)
      (do-with SimpleIO
        (say s)
        (call/cc (read))))

    ; And now we use our continuation:
    (:: ex9 ())
    (fn ex9 ()
      (do-with SimpleIO
        (name := (prompt "What is your name? "))
        (say (name :++ " is a wonderful name."))))

For the brave: delimited continuations in psilo are reminiscent of monads in
Haskell or Scala, except here they are designed to be easy to create,
understand, and use. Additionally they lend type-safety and allow for the
creation of EDSLs.

### Macros

Haven't decided this yet, sorry :)

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
