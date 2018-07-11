; A function to test out predicates
(= square (x) (* x x))

;; Bool
(::= Bool ()
  (forall (r)
    (-> r r r)))

(= my-if (c t e) ((~Bool c) t e))

(= true (Bool (\ (x y) x)))
(= false (Bool (\ (x y) y)))

;; Identity
(::= Identity (a)
  (forall (r) (-> (-> a r) r)))

(= identity (x) (Identity (\ (k) (k x))))
(= unIdentity (i) ((~Identity i) (\ (x) x)))

(= map-identity (f i) (identity (f (unIdentity i))))

;; Pairs
(::= Pair (a b)
  (forall (r)
    (-> (-> a b r) r)))

(= pair (x y) (Pair (\ (k) (k x y))))
(= fst (p) ((~Pair p) (\ (x y) x)))
(= snd (p) ((~Pair p) (\ (x y) y)))

(= test-pair-1 (pair 1.0 #t))
(= test-pair-2 (square (fst test-pair-1)))
(= test-pair-3 (snd test-pair-1))

;; Optional type
(::= Optional (a)
  (forall (r)
    (-> (-> a r) r r)))

(= maybe (o n j)
  ((~Optional o) j n))

(= map-optional (f o) (maybe o (null) (\ (x) (just (f x)))))

(= just (x) (Optional (\ (j n) (j x))))
(= null ()  (Optional (\ (j n) n)))

;; Either
(::= Either (a b)
  (forall (r)
    (-> (-> a r) (-> b r) r)))

(= either (e l r) ((~Either e) l r))

(= left (x) (Either (\ (l r) (l x))))
(= right (y) (Either (\ (l r) (r y))))

(= map-either (f e) (either e (\ (x) (left x)) (\ (y) (right (f y)))))

;; List
(::= List (a)
  (forall (r)
    (-> (-> a r r) r r)))

(= foldr (c e xs) ((~List xs) c e))

(= cons (x xs) (List (\ (c e) (c x (foldr c e xs)))))
(= nil () (List (\ (c e) e)))

(= map-list (f xs) (foldr (\ (y ys) (cons (f y) ys)) (nil) xs))

(= split (xs)
  (foldr (\ (y ys)
           (pair (just y)
                 (List (\ (c e)
                         (maybe (fst ys) e (\ (x) (c x (foldr c e (snd
  ys)))))))))
  (pair (null) (nil))
  xs))

(= head (xs) (fst (split xs)))
(= tail (xs) (snd (split xs)))

(= append (xs ys) (foldr cons ys xs))
(= concat (xs) (foldr append (nil) xs))

(= singleton (x) (cons x (nil)))

(= test-id-1 (identity 1.0))
(= test-id-2 (square (unIdentity test-id-1)))

;(: foo
;  (forall (a)
;    (-> a
;      (forall (b)
;        (-> (-> a b) b)))))
;(= foo (x) (\ (f) (f x)))
