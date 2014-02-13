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

    (let ((square (fn (x) (* x x)))
          (my-vector [1 2 3 4 5 6]))
      (square my-vector))

    ; => [1 4 9 16 25 36]

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

1. [ x ] Take a file as input, not a REPL
2. [   ] Basic type checking
3. [   ] Add unique types to the kind system
4. [   ] Algebraic data types (as closures)
5. [   ] We'll see?

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

Eventually, the goal is for the following to be a complete psilo program:

    ;; psilo has functions as you might expect
    (fn square (x) (* x x))
    
    ;; and algebraic data types
    (adt Person ()
      (Human (name : String)
             (age  : Int))
    
      (Corporation (name  : String)
                   (state : String)
                   (age   : Int)
                   (taxId : String)))
                   
    ;; You can overload functions, like the `show` function
    (:: show : Person -> String)
    (fn show ((Human name age))
      (concat name
              ", aged "
              (show age)
              " years"))
              
    (fn show ((Corporation name state age taxId))
      (concat name
              ", a "
              state
              " corporation; "
              (show age)
              " years old ("
              taxId
              ")"))
              
    ;; Idiomatically, you can mutate values like so
    
    ; a function which mutates a person
    (:: birthday : Person -> Person)
    (fn birthday ((Human name age))
      (Person name (+ 1 age)))
    (fn birthday ((Corporation name state age taxId))
      (Corporation name state (+ 1 age) taxId))
      
    (fn ex1 ()
      (let ((p (Human "gatlin" 25)))
        (let ((p (birthday p)))
          (Human->age p))))
    ; (ex1) => 26
    
    ;; In the previous example, because p was shadowed using itself,
    ;; it was updated in-place in memory.
    
    ;; To write imperative code, one must define a monad, like in Haskell.
    ;; In psilo we have tried to make this process more intuitive:
    
    (adt SimpleIO (a)
      (Print String a)
      (Read  (String -> a)))
      
    (free SimpleIO with-io
      (print (Print s k) (let ((_ (c-printf s)))
                           (with-io k)))
      (read  (Read k)    (let ((input (c-scanf)))
                           (with-io (k input)))))
                     
    (fn prompt (s)
      (do (print s)
          (set x (read))
          (return x)))
          
    ;; NB: there is a special syntax for set:
          
    (fn ex2 ()
      (with-io (do
        (name := (prompt "What is your name?"))
        (print (concat name " is a stupid name.")))))
        
    ;; psilo also has vectors, which are homogeneously typed aggregate values
    ;; laid out sequentially in memory. They may be manipulated by stream fusion,
    ;; and have a very special property, which I'll show by example:
    
    (let ((x (vector '(1 2 3))))
      (square x))
      
    ; => [1 4 9]
    
    ;; Here is something really interesting though:
    
    (adt Stream (a) (End) (Next a (Stream a)))
    
    (:: buffer : Int -> Stream a -> [a])
    (fn buffer (_ (End)) [])
    (fn buffer (n (Next h t))
      (concat [h] (buffer (- n 1) t)))
      
    (:: audio-xform : Int -> Int)
    (fn audio-xform (x) (...))
    
    ; and given some AudioIO monad ...
    
    (:: ex3 : AudioIO ())
    (fn ex3 ()
      (with-sensor-io (do
        (stream := (make-audio-input-stream params ...))
        (buf    := (buffer 1000 stream))
        (buf-2  := (audio-xform buf))
        (output-audio buf-2))))

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
