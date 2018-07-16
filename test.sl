;;;
; Syntax for new types:
;
;     (::= <name:symbol> (<variable:symbol> ...) <typeexpr>)
;
; This says that `(TypeName vars...)` is an alias for the type expression. Two
; phantom functions are generated, a constructor and a destructor. Their types
; are generated rather simply like so:
;
;     TypeName: ∀<variables>. <typeexpr> -> (<name> <variables>)
;     ~TypeName:  ∀<variables>. (<name> <variables>) -> <typeexpr>
;
; The constructor and destructor allow me to define recursive types without
; running afoul of the inferencer and creating an infinite type.
;
; With the help of "Church" / Scott / Boehm-Berarducci / etc. encodings, this is
; surprisingly powerful.

; Okay okay examples! First: the simplest data structure, the Box.
(::= Box (a) (forall (r) (-> (-> a r) r)))

; Its constructor and destructor are:
;
;     Box  : ∀a. ∀r. (a -> r) -> r -> (Box a)
;     ~Box : ∀a b. (Box a) -> ((a -> b) -> b)
;
; Note too that destructors are transformed a little so that the return types
; can be concrete and predicative.

; ∀a. a -> (Box a)
(: box (forall (a) (-> a (Box a))))
(= box (thing) (Box (\ (k) (k thing))))

; ∀a. (Box a) -> a
(= unbox (a-box) ((~Box a-box) id))

; ∀a b. (a -> b) -> (Box a) -> (Box b)
(= map-box (fn b) (box (fn (unbox b))))

; Let's try out our mapping

; Box Float
(= float-box (box 2.0))

; Box Boolean
(= boolean-box (map-box even-number? float-box))

;; More interesting example: the humble pair type
(::= Pair (a b) (forall (r) (-> (-> a b r) r)))

; ∀a b. a -> b -> (Pair a b)
(= pair (x y) (Pair (\ (k) (k x y))))

; ∀a b. (Pair a b) -> a
(= fst (p) ((~Pair p) (\ (x y) x)))

; ∀a b. (Pair a b) -> b
(= snd (p) ((~Pair p) (\ (x y) y)))

; ∀a b c. (b -> c) -> (Pair a b) -> (Pair a c)
(= map-pair (fn p) ((~Pair p) (\ (x y) (pair x (fn y)))))

; Pair Boolean Float
(= test-pair-1 (pair #f 5.0))

; Pair Boolean Float
(= test-pair-2 (map-pair square test-pair-1))

; Pair Boolean Boolean
(= test-pair-3 (map-pair even-number? test-pair-2))

; Boolean
(= test-pair-4 (snd test-pair-3))

;; The Mighty Optional type
(::= Optional (a)
 (forall (r) (-> r (-> a r) r)))

; And its two constructors: `just` and `null`.
; ∀a. a -> (Optional a)
(= just (x) (Optional (\ (n j) (j x))))
; ∀a. Optional a
(= null (Optional (\ (n j) n)))

; A cool thing about sum types like Optional is that their destructor is
; basically a control flow mechanism like `if`. Here I wrap it to rearrange the
; arguments a little.

; ∀a b. (Optional a) -> b -> (a -> b) -> b
(= maybe (o n j) ((~Optional o) n j))

; ∀a b. (a -> b) -> (Optional a) -> (Optional b)
(= map-optional (f o) (maybe o null (\ (x) (just (f x)))))

; Optional Boolean, for both
(= test-option-1 (map-optional even-number? (just 1.0)))
(= test-option-2 (map-optional even-number? null))

;; For completion here's the type called `Either a b` in Haskell:
(::= Either (a b)
  (forall (r)
    (-> (-> a r)
        (-> b r)
        r)))

; ∀a b c. (Either a b) -> (a -> c) -> (b -> c) -> c
(= either (e l r) ((~Either e) l r))

; ∀a b. a -> (Either a b)
(: left (forall (a b) (-> a (Either a b))))
(= left (x) (Either (\ (l r) (l x))))

; ∀a b. b -> (Either a b)
(= right (y) (Either (\ (l r) (r y))))

; ∀a b c. (b -> c) -> (Either a b) -> (Either a c)
(= map-either (f e) (either e (\ (x) (left x)) (\ (y) (right (f y)))))

;; Okay here's the Big Boy: The List Type
(::= List (a)
  (forall (r)
    (-> (-> a r r)
        r
        r)))
; ∀a b. (a -> b -> b) -> b -> (List a) -> b
(= foldr (c e xs) ((~List xs) c e))

; ∀a. a -> (List a) -> (List a)
(= cons (x xs) (List (\ (c e) (c x (foldr c e xs)))))

; ∀a. List a
(= empty-list (List (\ (c e) e)))

; ∀a b. (a -> b) -> (List a) -> (List b)
(= map-list (f xs) (foldr (\ (y ys) (cons (f y) ys)) empty-list xs))

; ∀a. (List a) -> (List a) -> (List a)
(= append (xs ys) (foldr cons ys xs))

; ∀a. (List (List a)) -> (List a)
(= concat (xs) (foldr append empty-list xs))

; ∀a. a -> (List a)
(= singleton (x) (cons x empty-list))

; This one is a little tricky.
; ∀a. (List a) -> (Pair (Optional a) (List a))
(= split (xs)
  (foldr
    (\ (y ys)
      (pair (just y)
            (List (\ (c e)
                    (maybe (fst ys)
                            e
                            (\ (x)
                              (c x (foldr c e (snd ys)))))))))
    (pair null empty-list)
    xs))

; ∀a. (List a) -> (Optional a)
(= head (xs) (fst (split xs)))

; ∀a. (List a) -> (List a)
(= tail (xs) (snd (split xs)))

; Here we define the left fold in terms of the right fold, which is a little
; tricky.
; ∀a b. (b -> a -> b) -> b -> (List a) -> b
(= foldl (fold-fn init-value list)
  ((foldr (\ (xs k) (\ (x) (k (fold-fn x xs))))
          (\ (x) x)
          list)
     init-value))

;; Hey I know let's mimic type classes with our newfound abilities.
; This won't work for types with more than one variable until I can get the
; *checking* and matching part of my type checker working. Then, a type
; annotation can be used to specify that (forall (a) (Pair a)) is the functor,
; not Pair.

(::= Functor (f)
  (forall (r)
  (->
    (-> (forall (a b) (-> (-> a b) (f a) (f b)))
        r)
    r )))

; ∀f. ∀a b. (a -> b) -> (f a) -> (f b) -> (Functor f)
(: functor (forall (f) (-> (forall (a b) (-> (-> a b) (f a) (f b))) (Functor ))))
(= functor (map-fn) (Functor (\ (k) (k map-fn))))

; ∀f a b. (Functor f) -> (a -> b) -> (f a) -> (f b)
(= map (fctor f x) ((~Functor fctor) (\ (fn) (fn f x))))

; Functor Box
(= box-functor (functor map-box))
(= optional-functor (functor map-optional))

; Not quite yet, but soon:
; (= pair-functor (functor map-pair))

;; Our little demonstration functions:
; ∀a. a -> a
(: id (forall (a) (-> a a)))
(= id (x) x)

; ∀a. (Integral a) => a -> Boolean
(: even-number? (=> ((Integral n)) (-> n Boolean)))
(= even-number? (n) (=? 0 (modulo n 2)))

; ∀a. (Num a) => a -> a
(: square (=> ((Num n)) (-> n n)))
(= square (x) (* x x))
