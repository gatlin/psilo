(= square (x) (* x x))

(= pair (a b)
  (\ (f) (f a b)))

(= two (+ 1 square))

(= fst (pr)
  (pr (\ (a b) a)))

(= snd (pr)
  (pr (\ (a b) b)))

(= pair-1 (pair 1 2))

(= maybe (m r o) (m r o))

(= just (x) (\ (j n) (j x)))
(= none ()  (\ (j n) n))

(= foldr (xs c n) (xs c n))

(= cons (x xs) (\ (c n) (c x (foldr xs c n))))
(= nil  ()     (\ (c n) n))


