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

(= id (x) x)

(: compose (forall (a b c) (-> (-> b c) (-> a b) (-> a c))))
(= compose (f g) (\ (x) (f (g x))))

;; The simplest data structure: a container of one value
(::= Box (a) (forall (r) (-> (-> a r) r)))

(: box (-> a (Box a)))
(= box (x) (Box (\ (k) (k x))))

(: unbox (-> (Box a) a))
(= unbox (b) ((~Box b) id))

;; Pairs
(::= Pair (a b) (forall (r) (-> (-> a b r) r)))

(: pair (-> a b (Pair a b)))
(= pair (x y) (Pair (\ (k) (k x y))))

(: fst (-> (Pair a b) a))
(= fst (p) ((~Pair p) (\ (x y) x)))

(: snd (-> (Pair a b) b))
(= snd (p) ((~Pair p) (\ (x y) y)))

;; Optional types
(::= Optional (a) (-> r (-> a r) r))

(: just (-> a (Optional a)))
(= just (x) (Optional (\ (n j) (j x))))

(: none (Optional a))
(= none (Optional (\ (n j) n)))

(: maybe (-> (Optional a) b (-> a b) b))
(= maybe (o n j) ((~Optional o) n j))

;; A variant type, Either
(::= Either (a b)
  (forall (r) (-> (-> a r) (-> b r) r)))

(: left (-> a (Either a b)))
(= left (x) (Either (\ (l r) (l x))))

(: right (-> b (Either a b)))
(= right (x) (Either (\ (l r) (r x))))

(= either (e l r)
  ((~Either e) l r))

;; Lists
(::= List (a)
  (forall (r)
    (-> (-> a r r)
        r
        r)))

; If you look closely you'll see that the definition of a List type looks like
; the definition of a right fold
(: foldr (-> (-> a b b) b (List a) b))
(= foldr (c e xs) ((~List xs) c e))

(: cons (-> a (List a) (List a)))
(= cons (x xs) (List (\ (c e) (c x (foldr c e xs)))))

(: empty-list (List a))
(= empty-list (List (\ (c e) e)))

(: append (-> (List a) (List a) (List a)))
(= append (xs ys) (foldr cons ys xs))

(: concat (-> (List (List a)) (List a)))
(= concat (xs) (foldr append empty-list xs))

(: split-list (-> (List a) (Pair (Optional a) (List a))))
(= split-list (xs)
  (foldr
    (\ (y ys)
      (pair (just y)
            (List (\ (c e)
              (maybe (fst ys)
                     e
                     (\ (x)
                       (c x (foldr c e (snd ys)))))))))
    (pair none empty-list)
    xs))

(: head (-> (List a) (Optional a)))
(= head (xs) (fst (split-list xs)))

(: tail (-> (List a) (List a)))
(= tail (xs) (snd (split-list xs)))

; The left fold doesn't need to traverse the entire list before processing,
; unlike the right fold.
(: foldl (forall (a b) (-> (-> b a b) b (List a) b)))
(= foldl (fold-fn init-value list)
  ((foldr (\ (xs k) (\ (x) (k (fold-fn x xs))))
          id
          list)
    init-value))

;; We can mimic typeclasses, too

(::= Functor (f)
  (forall (r)
    (-> (-> (forall (a b) (-> (-> a b) (f a) (f b))) r) ; map
        r)))

(= functor (map-fn) (Functor (\ (k) (k map-fn))))
(= map (fctor f x) ((~Functor fctor) (\ (fn) (fn f x))))

(: functor-box (Functor Box))
(= functor-box (functor (\ (f b) (box (f (unbox b))))))

(: functor-optional (Functor Optional))
(= functor-optional
  (functor (\ (f o)
    (maybe o none (\ (x) (just (f x)))))))

(: functor-either (Functor (Either a)))
(= functor-either
  (functor (\ (f e)
    ((~Either e) left (\ (x) (right (f x)))))))

(: functor-pair (Functor (Pair a)))
(= functor-pair
  (functor (\ (f p)
    ((~Pair p) (\ (x y) (pair x (f y)))))))

(: functor-list (Functor List))
(= functor-list
  (functor (\ (f xs)
    (foldr (\ (y ys) (cons (f y) ys)) empty-list xs))))

;; A Lens focuses on a constituent part of a given structure, allowing it to be
; inspected and modified.

(::= Lens (s t a b)
  (forall (f) (-> (Functor f) (-> a (f b)) s (f t))))

; What makes lenses interesting is that they compose.
(: . (-> (Lens b c d e) (Lens a f b c) (Lens a f d e)))
(= . (lens1 lens2)
  (Lens (\ (mapper f s)
    ((\ (l1 l2)
        (l2 mapper (\ (x) (l1 mapper f x)) s))
     (~Lens lens1)
     (~Lens lens2)))))

; Detour: Constant is like Box, except no matter how much you map over it, its
; internal value stays, well, constant.
(::= Constant (a b)
  (forall (r) (-> (-> a r) r)))

(: constant (-> a (Constant a b)))
(= constant (x) (Constant (\ (k) (k x))))

(: get-constant (-> (Constant a b) a))
(= get-constant (c) ((~Constant c) id))

; The explicit annotation is necessary because the wrong type will be inferred
; (for obvious reasons)
(: map-constant (-> (-> a b) (Constant c a) (Constant c b)))
(= map-constant (f c) c)
(= functor-constant (functor map-constant))

; With Box and Constant we can define two lens combinators

; This allows us to mutate whatever a lens focuses on (potentially changing the
; type of the structure)
(: over (-> (Lens s t a b) (-> a b) s t))
(= over (lens f s)
  (unbox ((~Lens lens) functor-box (compose box f) s)))

; This simply lets us view what the lens is focusing on.
(: view (-> (Lens s t a b) s a))
(= view (lens s)
  (get-constant ((~Lens lens) functor-constant constant s)))

; Example lenses to focus on the first and second parts of a pair
(: _1 (Lens (Pair a b) (Pair c b) a c))
(= _1 (Lens (\ (mapper f p)
  ((~Pair p) (\ (a b) (map mapper (\ (x) (pair x b)) (f a)))))))

(: _2 (Lens (Pair a b) (Pair a c) b c))
(= _2 (Lens (\ (mapper f p)
  ((~Pair p) (\ (a b) (map mapper (\ (x) (pair a x)) (f b)))))))

; Now let's use our lenses.
(: pair-1 (Pair Boolean Boolean))
(= pair-1 (over _1 even-number? (pair 3.0 #t)))
(= pair-2 (pair 2.0 (pair 3.0 #t)))

(: _1st_of_2nd (Lens (Pair d (Pair a b)) (Pair d (Pair c b)) a c))
(= _1st_of_2nd (. _1 _2))

(: a-float Float)
(= a-float (view _1st_of_2nd pair-2))

(: a-boolean Boolean)
(= a-boolean (view _1st_of_2nd (over _1st_of_2nd even-number? pair-2)))

;; oh shit what up
(::= IO (a)
  (forall (r)
    (-> (-> a r)
        (forall (t) (-> (FFI t) (-> t r) r))
        r)))

; Basically just Box, but it's Different
(::= FFI (a) (forall (r) (-> (-> a r) r)))

(: run-ffi (-> (FFI a) (IO a)))
(= run-ffi (ffi) (IO (\ (kp kf) (kf ffi kp))))

(: run-io (-> (IO a) (-> a r) (forall (i) (-> (FFI i) (-> i r) r)) r))
(= run-io (io a b) ((~IO io) a b))

(: functor-io (Functor IO))
(= functor-io
  (functor (\ (f io)
    (IO (\ (kp kf) (run-io io (\ (x) (kp (f x))) kf))))))

(::= Monad (m)
  (forall (r)
    (-> (-> (Functor m)
            (forall (a) (-> a (m a)))
            (forall (a b) (-> (m a) (-> a (m b)) (m b)))
            r)
        r )))

(= monad (functor-impl unit-fn bind-fn)
  (Monad (\ (k) (k functor-impl unit-fn bind-fn))))

(: unit (-> (Monad m) a (m a)))
(= unit (m x) ((~Monad m) (\ (f u b) (u x))))

(: bind (-> (Monad m) (m a) (-> a (m b)) (m b)))
(= bind (mnd m f) ((~Monad mnd) (\ (_ u b) (b m f))))

(: unit-io (-> a (IO a)))
(= unit-io (x) (IO (\ (kp kf) (kp x))))

(: bind-io (-> (IO a) (-> a (IO b)) (IO b)))
(= bind-io (m f)
  (IO (\ (kp kf) (run-io m (\ (a) (run-io (f a) kp kf)) kf))))

(= monad-io (monad functor-io unit-io bind-io))

;; Some functions for demonstration purposes
(= even-number? (n) (=? 0 (modulo n 2)))
