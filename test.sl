(= pair (x y)
  (\ (f) (f x y)))

(= fst (p)
  (p (\ (a b) a)))

(= snd (p)
  (p (\ (a b) b)))

(= p1 (pair 1 2))

(print (fst (p1)))
