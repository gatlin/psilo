(= id (x) x)
(= even-number? (x) (=? 0 (modulo x 2)))
(::= Functor (f)
  (forall (r) (-> (-> (forall (a b) (-> (-> a b) (f a) (f b)))
        r)
    r )))

(: functor (forall (f) (-> (forall (a b) (-> (-> a b) (f a) (f b))) (Functor f))))
(= functor (map-fn) (Functor (\ (k) (k map-fn))))

(= map (fctor f x) ((~Functor fctor) (\ (fn) (fn f x))))

(::= Pair (a b)
  (forall (r) (-> (-> a b r) r)))

(= pair (x y) (Pair (\ (k) (k x y))))

(= fst (p) ((~Pair p) (\ (x y) x)))

(= snd (p) ((~Pair p) (\ (x y) y)))

(: functor-pair (Functor (Pair a)))
(= functor-pair
   (functor (\ (f p)
     ((~Pair p) (\ (x y) (pair x (f y)))))))

(= p1 (pair 2.0 2.0))

(: p2 (Pair Float Boolean))
(= p2 (map functor-pair even-number? p1))

(= p3 (snd p2))

(= square (x) (* x x))
