(= Pair (p) p)
(= unpair (p f)
  (p f))

(= pair (x y)
  (Pair (\ (f) (f x y))))

(= fst (p)
  (unpair p (\ (a b) a)))

(= snd (p)
  (unpair p (\ (a b) b)))

(= map-pair (f p)
  (pair (fst p) (f (snd p))))

(= square (x) (* x x))

(let
  ((p1 (pair 1 2)))
  (print (snd (map-pair square p1))))
