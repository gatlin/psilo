psilo
=====

a parallel, safe, inferencing list operation language for writing interesting
programs.

(c) 2014 [Gatlin Johnson](http://niltag.net) <gatlin@niltag.net>

0. What is psilo?
---

psilo will be a programming language created with the philosophy that *all*
programs essentially define (restricted) languages.

Technical Features (planned):

- No run-time garbage collection necessary owing to uniqueness types
- Static typing for compile-time verification and optimization
- Malleable syntax with fexpr-esque macros
- Dead-simple parallelism with special array types
- Orthogonal core syntax and semantics for your performance and my sanity

Philosophical Features:

- All programs are parsers of some input language. psilo is carefully designed
  to promote this way of thinking and make it performant.

- That which is required should be elegant; that which is optional should be
  pragmatic.

- If it can be determined at compile time, it should be.

- Advanced computer science ideas should be exposed, but not mandatory for
  writing high quality programs.

- 

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

    (let ((square (\ (&:x) (* x x)))
          (add1   (\ (&:x) (+ 1 x))))
      (add1 (square 5)))

    ; => 26

    (let ((square (\ (&:x) (* x x)))
          (a      [ 1 2 3 4 5 ] ))
      (square a))

    ; => [ 1 4 9 16 25 ]

    (adt Stream (a)
      (Nil)
      (Next a (Stream a)))

    (= stream-length (&:strm)
      (= stream-length-helper (&:strm acc)
        (? strm
          (`(Nil)       0)
          (`(Next h ,t) (stream-length-helper t (+ 1 acc)))))
      (stream-length-helper strm 0))

2. Detail
---

### 2.1 Lists

A list is an ordered, heterogenously typed multiset delimited by parentheses.
They are akin to *tuples* in Python and other languages. Operationally a list
is simply several adjacent values in memory.

A list's type is the product of its constituent types, and it is equivalent
only to itself. Really, there is no list. It's a syntactic mechanism for
grouping values; lists do not exist at runtime.

### 2.2 Evaluation semantics

psilo is non-strict. In the basic case values are reduced only when and as much
as necessary. The upside to this is that, unless otherwise specified, functions
receive the expressions passed to them in the form of lists.

Since functions receive the expressions passed to them, you can use the `?`
operator to pattern match on the lists to grab pieces that you need. The
utility of this will be made plain soon.

It is possible to force evaluation, though the proper syntax has not been
decided yet.

### 2.3 Linear values and borrowing (it's simple!)

By default, closures and non-function values are *linear*: they must be used
exactly once in an expression.

This makes writing certain kinds of programs very annoying; eg

    (= square (x) (\* x x))

is illegal on the face of it. However, since a function receiving a linear
value is provably the only owner of that value, a function can declare a linear
value to be *borrowed* for the duration of its scope like so:

    (= square (&:x) (\* x x))

Borrowed values are immutable; you may think of them as equivalent to `const`
references in C++.

### 2.4 Mutation

Linearity is adopted in psilo so that programmers may mutate values
efficiently. In the core syntax of psilo:

    (let ((x (foo))           ; 1
          (y (bar)))          ; 2
      (let ((x (baz x y)))    ; 3
        (qux x)))             ; 4

The code above:

- Assigns values to the variables `x` and `y` (1 & 2);
- Consumes their values, permitting no further use (3);
- Replaces the value of `x` with the result (3 again); and
- Consumes `x` again (4).

Thus psilo supports mutation. As this is clumsy and difficult to think about,
the standard library defines constructs which more closely resemble a
traditional imperative language; we will get to that shortly.

### 2.5 Functions and Closures

#### 2.5.1 Function Basics

Functions are created with the `\` operator:

    (\ (x) (foo x))

Functions are referentially transparent: given some input value *x* a function
must always return *y*. No exceptions. Much of the rest of the language is
designed to get around this restriction without violating it. If one wishes to
perform side effects, one is going to *earn* them.

It is harmless to apply a non-function to an empty argument list, eg

    (let ((x (5)))
      (foo (x)))

In the example I

- evaluate the constant `5`; and
- evaluate the non-function variable `x`

with no problems.

#### 2.5.2 Type signatures

So far we have not used any explicit type signatures because they can almost
always be inferenced. However, if the need arises, one may type functions like
so:

    (\ ({x | Integer}
        {y | String }) -> Foo
      (make-foo x y))

Braces (`{` and `}`) delimit argument types. From the `|` until the closing
brace, you may write your type signature. psilo, like other languages with
expressive type systems, allows type variables as well:

    (\ ({x | a}) -> (Bar a)
      (bar-something x))

Also note the `->` symbol to indicate the function's return type.

To type variables in `let` bindings:

    (let (({x | Integer} 5)
          ({y | String } "huh"))
      (make-foo x y))

#### 2.5.3 `=` notation

Strictly speaking, all psilo programs are a single `let` expression being
evaluated:

    (let ((square (\ (&:x) (\* x x)))
          (add-1  (\ (&:x) (+ 1 x))))
      (square
        (add-1 5)))

This, however, is less than ideal syntactically. The following is equivalent:

    (= square (&:x) (\* x x))
    (= add-1  (&:x) (+ 1 x))

    (= main ()
      (square
        (add-1 5)))

(Actually, they're not *quite* equivalent: symbols bound via `=` form a
recursive `let`.)

#### 2.5.4 Closures

If a function returns an anonymous function, and that anonymous function
consumes any values from its environment, it is a closure. A closure contains
its own copy of its lexical environment; in fact, this is psilo's only native
copying mechanism at the moment.

Since closures are linear, if they are executed they cease to exist. However,
if the return value of a closure is another such closure, it may be reassigned
to the same symbol. Thus psilo provides a means of encapsulated mutation.

Note that the author is aware that this is a bit clumsy and not very friendly
to the programmer. Be patient; a web is being woven.

Example:

    ; a simple model of a person
    (= make-person (name-arg age-arg)
      (let ((name name-arg)
            (age  age-arg))
        (\ (msg)
          (? msg
            ('birthday    (let ((age (+ 1 age)))
                            (make-person name age)))
            ('say-hello   (let ((_ (display (++ "Hello, my name is "
                                                &:name
                                                " and I am "
                                                (show &:age)
                                                " years old."))))
                            (make-person name age)))))))

    (= person-example ()
      (let ((p (make-person "gatlin" 24)))
        (let ((p (p 'birthday)))
          (p 'say-hello'))))

    ; output: "Hello, my name is gatlin and I am 25 years old."

In the above code, `person-example` creates a person, mutates that person, and
then performs an effectful computation with it (him?).

#### 2.5.5 Macros (?)

Metaprogramming is awesome and you should do it. However, the author is not
satisfied with the various warts on existing macro / syntax extension systems.

As mentioned elsewhere, in psilo expressions are not evaluated until necessary.
Using the backquote operator (`\``) you can quote expressions and use the
unquote (`,`) operator to unquote values of interest. Thus, functions are a
kind of first-class macro (or *fexpr* in some parlances).

Just for kicks, let's add lisp-style `if` statements to psilo:

      (= if (&:cond &:then &:else)
        (? (cond)
          (`(True)    (then))
          (`(False)   (else))))

      (= if-example ()
        (if (< 2 4)
            "2 is less than 4!"
            "2 is NOT less than 4! Everything is wrong RUN"))

Note that the first argument to `?` has parentheses, because we want to
evaluate it. Since you can evaluate non functions all day long with no
problems, we defensively evaluate `then` and `else` in their respective cases.

Wasn't that simple? We blur the lines between code and data even further. The
author humbly accepts alternate syntax suggestions.

### 2.6 Data types

The above pattern is so useful that psilo provides its logical successor: the
algebraic data type. Examples are the most illuminating definition:

    (adt Person ()
      (Person String Integer))

    (= birthday (person)
      (? (person)
        (`(Person ,name ,age)     (Person name (+ 1 age)))))

    (= say-hello (&:person)
      (? (person)
        (`(Person ,name ,age)
          (let ((_ (display (++ "Hello my name is "
                                 name
                                 " and I am " (show age) " years old."))))
            (Person name age)))))

    (= person-example ()
      (let ((p (Person "gatlin" 24)))
        (let ((p (birthday p)))
          (say-hello p))))

Of course, ADTs may have multiple value constructors, eg:

    (adt Person ()
      (Human String Integer)         ; name age
      (Corp  String Integer String)) ; name age tax-id

    (= birthday (person)
      (? (person)
        (`(Human ,name ,age)  (Human name (+ 1 age)))
        (`(Corp  ,name ,age ,tax-id) (Corp name (+ 1 age) tax-id))))

ADTs may also accept type parameters to do useful things:

    (adt Stream (a)
      (Nil)
      (Next a (Stream a)))

    (= stream-length-helper (&:strm acc)
      (? (strm)
        (`(Nil)       0)
        (`(Next h ,t) (stream-length-helper t (+ 1 acc)))))

    (= stream-length (&:strm)
      (stream-length-helper strm 0))

    (= stream-example ()
      (let ((s (Next 1 (Next 2 (Next 3 (Nil))))))
        (let ((len (stream-length s)))
          (++ "The stream has "
              (show len)
              " elements."))))

### 2.7 Delimited continuations, `do`, and program-as-language

This is potentially the weirdest part of psilo. The diagonal code above can get
pretty annoying. It would be much nicer if there were `begin` blocks like in
other lisps.

Semantically, psilo does not have state or execution ordering beyond
mathematical function composition (eg, `f(g(x))` first performs `g` then `f`).

However, this does not mean we cannot build it ourselves. **You do not have to
understand all of the following code to actually use psilo.**

    ;;; Provided in standard library by yours truly
    (language I (Then I))

    (= imperatively ({imp | I a}) -> a
      (? imp
        (`(Terminal ,v)   v)
        (`(Then   ,next)  (imperatively (next)))))

    (= begin (exprs)
      (imperatively (do exprs)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; YOU WOULD WRITE THIS PART

    (= square-imperative (&:x)
      (return (* x x)))

    (= imperative-example ()
      (begin
        (x := 5)
        (x := (do (square-imperative x)))
        (display (show x))))

Yes, the one exception to psilo's prefix notation is the `:=` operator. In
fact this is not an exception: any operator may be moved one place to the right
one position in an expression.

Thus, using only native psilo code, we have created imperative programming. But
how?

#### 2.7.1 Languages

Follow me for a moment. Languages are often specified in Backus-Naur Form
(BNF), eg:

    arithmetic ::= <number>
                 | + <arithmetic> <arithmetic>
                 | * <arithmetic> <arithmetic>
                 | - <arithmetic>
                 | ( <arithmetic> )
    ... etc

In psilo, you can create a language similarly with ADTs:

    (adt Arithmetic ()
      (K    Integer)
      (Add  Arithmetic Arithmetic)
      (Mult Arithmetic Arithmetic)
      (Neg  Arithmetic))

    (= calc ({arith | Arithmetic}) -> Integer
      (? arith
        (`(K ,n)    (n))
        (`(Add ,l ,r)  (+ (calc l)
                          (calc r)))
        (`(Mult ,l ,r) (* (calc l)
                          (calc r)))
        (`(Neg  ,n)    (* n -1))))

    (= calc-ex () -> Integer
      (Add (K 1)
           (Mult (K 2)
                 (K 3))))

What if you wanted an imperative control language, say, for a robotic camera?

    ; assume types for Degree, ImageData, etc
    (adt Rotation ()
      (Up Degree) (Down Degree) (Left Degree) (Right Degree))

    (language CameraInst k
      (Shoot    (ImageData -> k))
      (Rotate   Rotation k)
      (SetZoom  Float  k))

    (= with-camera ({&:camera    | Camera    }
                    {instruction | CameraInst}) -> ()
      (? instruction
        (`(Term v)      ())

        (`(Shoot ,next)
          (let ((d  (tell-camera-shoot camera)))
            (with-camera camera (next d))))

        (`(Rotate ,rot ,next)
          (let ((_ (tell-camera-rotate camera rot)))
            (with-camera camera (next))))

        (`(SetZoom ,amt ,next)
          (let ((_ (set-camera-zoom camera amt)))
            (with-camera camera (next))))))

    (= camera-ex ()
      (with-camera (new-camera) (do
        (SetZoom 14.0)
        (Rotate (Up 23))
        (Rotate (Left 42))
        (image := (Shoot))
        (image))))

    (= camera-ex-2 ()
      (begin
        (which-camera := (ask-user-for-camera))
        (if (eq? which-camera "")
            (display "Invalid choice.")
            (begin
              ((with-camera (get-camera which-camera)
                (do (SetZoom 4.0)
                    (Rotate (Right 23))
                    (image := (Shoot))
                    (image))))))))

The psilo philosophy is that all programs are parsers for some input language,
be it another programming language, a language of clicks, a language of sensor
values, etc. Thus, the primitives of psilo have been carefully chosen to
promote this style of development.

S-expressions promote viewing a program as definitions and parsers of syntax
trees. Types allow you to restrict your parsers to particular languages. And
linearity allows you to do this efficiently.

#### 2.7.2 A little derring-`do`

`do` appears to be magical, but it is quite simple: it takes a list of values
of the equivalent type and composes a delimited continuation out of them. What
psilo calls continuations is called a monad in other languages; however, the
author finds their usage is more readily obvious if they are called
continuations.

The result of `do` is an unevaluated, composite expression. In the camera
example, we wrote a `with-camera` function which accepted a `CameraInst` value,
peeled it apart, and performed side-effects.

At no point have we violated referential transparency. Rather, we have built an
impure, effects-driven language out of a pure one which gives us the ability to
reason about it and ensure its safety.

### 2.7.3 Arrays

Parallelism in psilo is dead simple thanks to a fundamental type called the
array. Arrays are ordered, homogenously-typed, fixed-length multi-sets. They
may be created like so:

    (= array-ex ()
      (begin
        (arr := [1 2 3])
        (do-something-with arr)))

An interesting property of arrays is, if given to a function expecting a scalar
(ie, non-array) value, the operation is intelligently mapped in parallel, vis:

    (= square ({&:x | n}) -> n
      (* x x))

    (= square-array ({arr | [n]}) -> [n]
      (square arr))

    (square-array [1 2 3]) ; => [1 4 9]

Note how arrays are specified in type signatures. By explicitly typing the
argument as an array, you can prevent the parallel mapping behavior.

What about something more interesting?

    (= array-ex-2 ()
      (+ [1 2 3]
         [4 5 6 7]))

    ; => [5 6 7 8 6 7 8 9 7 8 9 10]

The resulting array is the cartesian product of the inputs reduced according
to the scalar-wise semantics of the function.

#### 2.7.3.1 GPUs

At some point in the future, I would like for this to do the obvious correct
thing:

    (scalar-function 'gpu [1 2 3])
    ; => Perform the parallel operation on the GPU, if applicable.

### 2.8 Quotes and other list miscellany

Lisp means **Lis**t **P**rocessor. While psilo has an unconventional take on
what that means, traditional quote operators still apply.

You can quote symbols:

    (foo 'x)

You can quote lists to indicate the expression head is not intended to be a
function:

    '(1 2 3)

And, as you've seen, you can unquote quoted lists using special syntax:

    `(a ,b c)

The quasiquoted list is used by the `?` operator to extract pattern matches and
control evaluation. I have not yet worked out all the implications of this
construct; more to come.

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

Use the Issues feature of GitHub or email me: <gatlin@niltag.net>.

[parsec]: http://hackage.haskell.org/package/parsec

[mu]:
http://debasishg.blogspot.com/2012/01/learning-type-level-fixpoint-combinator.html

[comonads]: http://brianmckenna.org/blog/type_annotation_cofree

[haskellplatform]: http://haskell.org/platform
