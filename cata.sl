(= square (x) (* x x))

(= pair (a b)
  (\ (f) (f a b)))

(= two (+ 1 square))

(= fst (pr)
  (pr (\ (a b) a)))

(= snd (pr)
  (pr (\ (a b) b)))

(= pair-1 (pair 1 2))
