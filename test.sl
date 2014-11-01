(= Pair (p) p)
(= unpair (p f)
  (p f))

(= pair (x y)
  (Pair (\ (f) (f x y))))

(= fst (p)
  (unpair p (\ (a b) a)))

(= snd (p)
  (unpair p (\ (a b) b)))

(let
  ((p1 (pair 1 2)))
  (print (fst p1)))
