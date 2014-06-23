psilo
=====

a parallel, safe, inferencing list operation language for writing interesting
programs. [View it on GitHub.](https://github.com/gatlin/psilo)

&copy; 2014 [Gatlin Johnson](http://niltag.net) <gatlin@niltag.net>

What is psilo?
===

psilo will be a programming language created with the philosophy that *all*
programs essentially define (restricted) languages.

Technical Features (planned):

- No run-time garbage collection necessary owing to uniqueness types
- Static typing for compile-time verification and optimization
- Malleable syntax with fexpr-esque macros
- Dead-simple parallelism with special array types
- Orthogonal core syntax and semantics for your performance and my sanity

Philosophy:

- All programming is manipulating languages.
- Types define grammars; functions define parsers.
- The earlier a question may be answered, the better.
- If the computer can do it, it should.

Status
===

**17 June 2014** Algebraic Data Types are going to be more like GADTs in
Haskell and the syntax reflects this. The symbol for creating ADTs is
reminiscent of EBNF as well.

**10 June 2014**  The syntax and semantics for lists and types have been
dramatically changed and simplified. Essentially, structs are quoted lists
which takes advantage of a number of other features to make ADTs and
continuations much simpler. I have also discarded the syntax for type
annotations and will revisit it.

**29 May 2014** A nascent evaluator is now committed. By no means is it
complete, but I can successfully run computations in a rough subset of the
language, so that's something!

The parser is not done but I am now on much better footing than I was before so
I can continue with it as planned.

Also the source code is now Literate Haskell as I want the psilo compiler to
also be a human-language guide to the language's structure and implementation.

Synopsis
===

The grammar is a work in progress. At the moment, psilo code looks like this:

    (let ((square (\ (x:&) (* x x)))
          (add1   (\ (x:&) (+ 1 x))))
      (add1 (square 5)))

    ; => 26

    (let ((square (\ (x:&) (* x x)))
          (a      [ 1 2 3 4 5 ] ))
      (square a))

    ; => [ 1 4 9 16 25 ]

    (::= Stream (a)
      (: Empty ())
      (: Cons  (a (Stream a))))

    (= stream-length (strm:&)
      (= stream-length-helper (strm:& acc)
        (? strm
          ('(Nil)      0)
          ('(Cons h t) (stream-length-helper t (+ 1 acc)))))
      (stream-length-helper strm 0))

    (let ((s    '(Cons 1 (Cons 2 (Cons 3 (Nil))))))
      (stream-length s))
    ; => 3

Detail
===

Basics: Expressions, primitive types, and functions
---

### Expressions

All psilo programs are contained within at least one *expression*. An
expression is a list of values delimited by parentheses, eg:

    (1)

    (some-function 2 3)

When a psilo program is evaluated, the first element of the list is taken to be
an *operator* and the remaining elements are its *operands*. However, this
default behavior may be changed; see the section on lists.

### Primitive types

psilo has pretty standard primitive data types: integers, floating point
numbers, long integers, short integers ascii characters, unicode characters,
and of course strings (though we'll get back to that).

Another type found mostly in the lisp family of languages is the *symbol*. A
symbol is a string of alphanumeric and selected punctuation characters. Symbols
evaluate to themselves and themselves alone unless they are *bound* to some
other value (see the section on functions).

Values of these types may be used as operands in psilo expressions.

### Functions

Functions transform lists of psilo values into some result value. They may be
both operators and operands in expressions.

You may create functions in two ways:

Anonymously:

    (\  (argument1 argument2)
      (operator ...))

Named:

    (= foo  (argument1 argument2 ...)
      (operator ...))

Really, the second form is a syntactical convenience for the first one, but
both have their uses. We will start by discussing anonymous functions to
motivate psilo's design decisions.

### Binding values to symbols

In the definition of a function you provide a list of symbols to bind to the
operand values. These symbols now refer to their bound values for the duration
of the function's *scope*, defined by its enclosing parentheses.

For example,

    (\ (x) (+ 1 x))

This takes a value (probably a numeric one) and binds it to the symbol `x`.
Then, `x` is used to compose a new expression - in this case, adding `1` to the
value bound to `x`. The resulting incremented value is the *return value* of
the function.

### Scope

psilo is *lexically scoped*. This means the following function is completely
disambiguous:

    ((\ (x y)
      (y x))

     (\ (x)
       (foo x)))

The function at the bottom binds some value to `x` in order to generate a
result value that will then be bound to `x` in the top function. `x` means two
different things here. When a function is applied all the symbols in its scope
become unbound, and if they were bound to values in the outer scope they revert
accordingly.

### Functions are values

Functions may also be used as the operands of expressions. For example:

    (\  (f  x)
      (f (foo x)))

`f` is used as an operator in the body of the function, which means `f` must be
a function. But `f` was passed in as an *operand* argument. This demonstrates
the distinction between operators and operands: a function may be either one in
context.

You don't have to use bound functions as operators, though: it is perfectly
legal to write a function literal as an operator, like so:

    (   (\ (f x)
          (f (foo x)))
      (\ (y)
        (bar y))

      2)

This is an unreadable mess and **I do not recommend writing code in this
style** but nonetheless it demonstrates the full implications of functions
being values and bound symbols.

### Linear types

psilo has a quirk that affords the programmer both incredible power and
incredible frustration: linearly typed values.

Unless otherwise demarcated, values in psilo are *linear*. This means they must
be used exactly once in an expression. Examples will make this clearer:

    ; legal
    (\ (x) (+ 1 x))

    ; illegal
    (\ (x) (* x x))

    ; legal
    (\ (x)
      (\ (y)
        (foo x y)))

    ; illegal
    (\ (x)
      ((\ (y)
         (foo x y))
       x))

Thus, when a function consumes a linear value, that value is no longer
available outside the function. As we will see later, though, there are some
very clever tricks to get around this and as a result psilo code is much easier
to verify and requires no manual memory management *or* run time garbage
collection.

The exception to this rule is that functions, **unless they are closures**, may
be referenced more than once in their enclosing scope. See the section on
closures for more information.

### Sharing

The inability to write a simple squaring function would be a devastating
oversight in the language's design; fortunately, there is a way to get around
this.

If a symbol has the suffix `:&` (eg, `x:&`), then it is a *reference*. For the
duration of the enclosing scope the symbol may be referenced as much as
desired, but it may **not** be mutated.

You can think of the `:&` as a request to borrow an item: a good neighbor might
borrow a tool, but great care must be taken not to damage or radically alter
the tool lest it cause confusion to its owner.

When the scope is discarded, all references are destroyed.

### `let` syntax and mutation

At this point it may not be clear how one can mutate linear values. This is a
great segue into a very commonly used syntactic form: `let` bindings. Examples:

    (let    ((x 5)
             (y 6))
      (* x y))

    (\  (id:&)
      (let ((name:&  (get-name id))         ; name is shared
            (balance (get-balance id)))     ; balance is linear
        (close-account? name balance id)))

To mutate a value, it's this simple:

    (let ((x (initial-value)))
      (let ((x (foo x)))
        (bar x)))

Shadowing a linear value mutates it. This is psilo's core mutation construct,
though there are others built on top of it as you will see.

Lists
---

### Motivation

Strictly speaking, all functions in psilo are *unary*: that is, they accept one
argument value. This affords a number of advantages you will see later.

However, for flexibility, that one argument is actually a compound value called
a *list*. A list may contain any psilo expression. Its type is the *product* of
the types of its members.

So the following function's argument list has a type similar to `(Integer
Integer)`:

    (\ (x   y)
      (* x y))

Lists are psilo's core compound data type, akin to structs or product types in
other languages.

### Lists are not actually real

Under the hood a list is really just a way of grouping related values on the
computer's function call stack (real or simulated). However this makes lists
very powerful: they only exist at compile time and **thus do not incur any sort
of run time penalty.**

### Create lists by quoting

Every function has an anonymous list containing its arguments, and that list is
broke up into its constituent parts so they may be evaluated and reduced to
some return value.

But what if I want to pass a list as an argument without it being a nested
expression? I can *quote* it.

Quoting a list prevents it from being evaluated as a list. Example:

    (foo 1 (* 2 3))  ; (1)

    (foo 1 '(* 2 3)) ; (2)

In (1), the number `6` is passed as the second argument to `foo`. In (2), the
list `(* 2 3)` is passed as the second argument.

We get the value of a quoted list by evaluating it, eg:

    (= produce ()
      (consume '(foo 1)))

    (= consume (expr)
      (expr))

    ; => computes `(foo 1)`

### Manipulating lists

If we halt evaluation we must want to do something with the contents of the
list, no? There are two core functions which can manipulate lists: `head` and
`tail`.

`head` returns the item at the beginning of the list, and `tail` returns the
remaining elements.

A simple use case might be passing in a slate of three functions to run on an
initial argument value.

### The empty list

`()` is called the *empty list*. It is the last element of every list and is
equivalent only to itself. Its sole purpose is to denote the end of a list so
that recursive functions may be written on lists without fear of memory issues.

### Quoting from the other direction

The other way to quote a list is in the callee's argument list. To motivate
this syntax, I will write a short-circuiting `and` function:

    (=  and/s (left   'right)
      (if (left)    ; evaluate the left side
          (and  left (right))
          (True)))

We have not spoken about Boolean types yet, but the example should make sense
regardless: there is no need to evaluate the right hand side if the left hand
side is true.

Our function may be used like so:

    (and/s  (and/s x y)    (and/s y z))

This is much cleaner looking for the caller. This form of quoting may be
thought of as automatically wrapping the value in an anonymous function.

### Quoting references

The quote syntax and the reference syntax each go on different sides of
argument symbols for a reason. This is perfectly valid:

    (\ ('x:&)
      (foo x))

### Quasi-quoting.

What if you want to construct a list which captures values from its surrounding
environment? Eg,

    (= say-hi (to from)
      (++   "Hello, "
            to
            ", it is I: "
            from
            "!"))

    (= make-say-hi    (name who-you-are)
      `(say-hi  ,name ,age))

So now the following is possible:

    (let ((greeting     (make-say-hi "Alice"    "Bob")))
      (do-something-with greeting))

    (= do-something-with (g)
      (display (greeting)))

Closures
---

### Basics

Closures are functions which *close over* their surrounding environment. An
example:

    (=  make-person (name:&     age:&)
      (\ ()
        (++ "Hello, my name is "
            name
            " and I am "
            (show age)
            " years old.")))

The inner anonymous function does not bind any symbols, so `name` and `age` are
*free* in the body of the inner function. However, the function is not
immediately evaluated: it is passed back as a value. In order for this to work,
it closes over the symbols `name` and `age`, which are immutable anyway.

If a function closes over a linear value like so, a deep copy is made:

    (= make-thing   (x)
      (\ ()
        (foo x)))

`make-thing` is not a closure and may be called as many times as necessary. The
resulting closure values, however, have copies of the relevant slices of their
environment.

### Subverting linearity

The thing about closures, though, is that they have memory allocated to them.
For a variety of reasons this means they must be linear, and linear values must
be evaluated exactly once.

So a closure can only be used once. This sounds incredibly stupid, sure, but it
is the price we pay.

However, a little cleverness can get around that problem:

    (=  make-person (name     age)
      (\ (what)
        `(,what ,name ,age)))

    (=  use-person  ()
      (let  ((p (make-person "gatlin" 25))
             (birthday  (\  (name     age)
                          (make-person name (+ 1 age)))))
        (let ((p    (p birthday)))
          (foo p))))

`p` is a closure, so it, `name`, and `age` are all linear. However in this
example I create a value representing a person and pass it a function which
calls `make-person` with modified values. This resulting closure is used to
shadow `p`.

Shadowing a linear value mutates the value. Thus we have safely mutated
encapsulated data.

This is a bit like the original notion of an object described by Alan Kay: the
object listens for messages and dispatches based on the message. Because of the
linearity restrictions, the mutated value are encapsulated inside the closure;
we can only actually retrieve them when we are ready to destroy the closure.

### Quotes and closures

The quote operators are actually just nice ways to construct ad-hoc closures.

    (\  ()
      (foo x y))

and

    '(foo x y)

Are essentially the same. Similarly,

    ((\ (x y)
       (\ ()
         (foo x y)))
     value-1 value-2)

and

    `(foo ,x ,y)

do basically the same thing. The point, though, is that the quote operators
allow you to more easily manipulate these expressions before evaluating them,
giving you a lot of control and expressivity.

Algebraic Data Types
---

### Prelude: structures

If you just want some tagged structure of data, the quote operators are
sufficient:

    '("gatlin" 25)

    '("washington" 282)

And you can always break them apart:

    (head '("gatlin" 25)) ;      => "gatlin"
    (tail '("washington" 282)) ; => '(282)

Or even:

    (let ((name (read-from-stdin)))
      `(name ,name))

to build a linear structure which captures its environment.

### Introduction to ADTs

An algebraic data type is a *sum* of *product* types. More plainly, it is a
type which can be one of several different list types. They are essentially a
generalization of the technique shown in the "Subverting linearity" section.

To give a simple idea of how to create ADTs, we will create an Optional type.
An `Optional` value will be one which either has a value, or is explicitly
devoid of value. This is useful for catching and managing failure, among
others. Here is the code:

    (::= Optional (a)
      (: Nil    ())
      (: Some   (a)))

Here, we name our type `Optional` and give it a *type variable*, here called
`a`. It could have been any legal symbol starting with a lower case letter,
though.

We then defined two *value constructors* for our type: `Nil` and `Some`. `Nil`
always produces an `Optional a` value, whereas `Some` is a function which takes
a value of type `a` and returns a value of `Optional a`.

Yes, you guessed correctly: value constructors are just functions automatically
generated for you. Let's use our new type:

(= optional-example ()
  (let ((name   (read-from-stdin)))
    (if ((length? name) > 0)
      `(Some ,name)
      '(Nil))))

Here, we read some data from stdin. If the data did not meet our requirements,
we return `Nil` so that the caller can receive explicit notice. Otherwise, we
return `Some` name. Note the uses of the two kinds of quotes: we quasiquote an
expression to capture values from the environment, and use a normal quote to
create a value which doesn't have an environment.

### Example 1

Let's make more people!

    (::= Person
      (: Human (String Integer))
      (: Corp  (String Integer String)))

    (= birthday (p)
      (? p
        ('(Human name age)       '(Human name (+ 1 age)))
        ('(Corp name age tax-id) '(Corp name (+ 1 age) tax-id)))))

    (= show-yourself (p)
      (? p
        ('(Human name age) (display
                             (++ "Name: "
                                 name
                                 ", age: "
                                 (show age))))
        ('(Corp name age tax-id)
          (display
            (++ "Corp Name: "
                name
                ", founded "
                (show age)
                " years ago. ("
                tax-id
                ").")))))

    (let ((h    '(Human "gatlin" 25))
          (c    '(Corp  "Buy-N-Large" 100 "123456789")))
      (let ((h (birthday h))
            (c (birthday c)))
        (show-yourself h)))

This will display a human with an incremented age.

### Example 2

As another example, let's create a Boolean data type and some convenient
utilities for it:

    (::= Boolean  ()
      (: True  ())
      (: False ()))

    ; there is a builtin though
    (= if/new   (condition 'then 'else)
      (condition    (\  ()  (then))
                    (\  ()  (else))))

    ; example usage
    (let ((x (get-some-value)))
      (if/new (=? x 2)      ; this returns True or False
          (foo x)
          (error "x is not 2")))

The `if` expression we just created does what you would expect: it defers
evaluation of either branch until it knows which to take.

### `?` operator and quoting

*Note: while all of psilo is a work in progress, quasi-quoting is possibly the
least developed of my ideas. As is I could make it work but I admit I don't
have a rock solid theoretical foundation quite yet. Just accept it for now.*

The `?` operator provides some syntactic convenience for writing case-wise
evaluation functions of algebraic data types. The above code could be rewritten
thusly:

    (= if/new   (condition 'then 'else)
      (? condition
        ('(True)    (then))
        ('(False)   (else))))

Or for another example:

    (::= Stream   (a)
      (:    End (Stream a))
      (:    Cons (-> a (Stream a) (Stream a))))

    (= stream-length    (strm:&)
      (? strm
        ('(End)      0)
        ('(Cons h t) (+ 1 (stream-length t)))))

`?` checks to see if there is some set of substitutions which makes the target
(here, `strm`) equivalent to any of the cases. Since symbol equality is rather
narrow, we essentially get a nice pattern matching syntax.

At the moment, `?` is a bit magic. I'm trying to think of how I might make it
less so.

Continuations
---

### Introduction

We still have a problem: there is no way to write imperative programs. The
following would be nice to have sometimes, though:

    (= f1   ()
      (begin
        (foo x)
        (bar y)
        ...))

For that matter, it would be cool also if you could supply your own semantics;
perhaps, given some `Optional` type, you want to be able to write code that
immediately exits on first failure:

    (= f2   ()
      (optionally (do
        (set v1 (potentially-works-1))
        (set v2 (potentially-works-2))
        (foo v1 v2))))

If either `v1` or `v2` have a null value, then the computation is aborted and
returns a null value; otherwise, the computation proceeds normally.

Between the statelessness, the linear values, and the strong type system, this
sounds about impossible. If there is no state, how can you order commands? The
answer lies in continuations.

### What are continuations?

So in traditional continuation-based programming, the continuation is some
(explicit or implicit) argument at the end of a function list, to which the
result value is passed:

    (=  f1  (a1 a2 k)
      (k (foo a1 a2)))

Think of continuation-based programming as having your "return" function passed
to you as a parameter:

    (= f1   (a1 a2 return)
      (return (foo a1 a2)))

This might elucidate its purpose.

In many languages, like Scheme, `k` is not present in the argument list but it
may be captured like so:

    (define (f1 a1 a2)
      (call/cc (lambda (k)
        (k (foo a1 a2)))))

`call/cc` stands for "call with current continuation."

Counterintuitively, despite psilo's emphasis on continuations they are not
hidden-but-present in all functions. Instead, you must define and use them.

### But abstract math *does* have a way to order things

In algebra, the following expression does in fact have an order of evaluation:

    f(g(x))

`g` must be evaluated first, and then `f` is evaluated with its result. As long
as the output of `g` is compatible with the input of `f` then we have
sequential execution.

If you can ensure that your functions meet certain type restrictions, then you
can create continuations and compose arbitrary functions together, which will
be executed however you like.

### Example: creating `begin`

A continuation is like defining an embedded language, and this language must
specify commands. As a motivating example, I will create a continuation with
one simple command: `Then`.

    (::=  Then    (k)
      (:    Then (-> k (Then k))))

The language is a simple algebraic data type, and it has one production rule:
sentences are composed of nested `Then`s. The `k` is a placeholder for the
continuation (pronounced with a hard "k" sound).

Having specified our language, we must write an interpreter function, which I
will call `imperatively`:

    (continuation   imperatively (Then)   (expr)
      (? expr
        ('(Term v) (v))
        ('(Cont k)
          (? k
            ('(Then next)  (imperatively (next))))))

A **lot** is going on under the hood here. Suffice it to say, the
`continuation` operator consumes programs written in our language but how do we
write them?

Continuations are constructed using `do`. `do` takes lists of compatibly-typed
expressions and constructs the continuation. To evaluate your embedded program,
you call the continuation function you wrote.

So the definition of `begin` is actually quite simple:

    (= begin    (exprs)
      (imperatively (do exprs)))

And to use it:

    (=  square  (x:&)
      (* x x))

    (=  add-1   (x)
      (call/cc (k)
        (k (+ 1 x))))

    (=  example ()
      (begin
        (set x 5)
        (set x (add-1 x))
        (set x (yield
                 (square x)))
        (display x)))

Note the addition of a few new operators. Any function may be "lifted" into the
continuation provided its return value is passed through `yield`, in the case
of `square`. `add-1`, however, invokes the continuation internally. The
advantage is, it may decide to escape into a different continuation
temporarily.

Since the continuations are delimited in extent, they are called *delimited
continuations.*

Vectors
---

Lastly, I will discuss vectors. Vectors are delimited by `[` and `]`. All the
elements must have some unifying type.

Vectors greatly simplify parallelism in psilo. If a vector of values of type
*a* are passed to a function accepting a single *a* then the function is
automatically mapped in parallel over the vector. Example:

    (=  square  (x:&)
      (* x x))

    (= vector-example ()
      (begin
        (set v  [1 2 3])
        (set v  (square v))
        (v)))

    ; yields [ 1 4 9 ]

Depending on the types and other annotations which I am still designing, this
will allow either automatic threading or automatic GPGPU computation.

This will definitely receive more elaboration in the future.

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
