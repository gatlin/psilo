(::= Pair (a b)
  (forall (r) (-> (-> a b r) r)))

(= pair (x y) (Pair (\ (k) (k x y))))

(= fst (pr) ((~Pair pr) (\ (x y) x)))
(= snd (pr) ((~Pair pr) (\ (x y) y)))

(= p1 (pair 1.0 #t))

(= p1-fst (fst p1))

(: p1-snd Float)
(= p1-snd (snd p1))

(= p1-test-good (* 2.0 p1-fst))

(::= D (a)
  (forall (r) (-> (-> a r) r)))

;(: d (-> a (D a)))
(= d (x) (D (\ (k) (k x))))

;(= p1-test-bad  (* 2.0 p1-snd))

;(: foo
;  (forall (a)
;    (-> a
;      (forall (b)
;        (-> (-> a b) b)))))
;(= foo (x) (\ (f) (f x)))
