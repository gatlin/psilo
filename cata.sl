(= id (x) x)

(= dup (x) (pair x x))

(= const (x)
  (\ (y) x))

(= if (c t e) (c t e))
(= T (x y) x)
(= F (x y) y)

(= square (x) (* x x))

(= pair (a b)
  (\ (f) (f a b)))

(= fst (pr)
  (pr (\ (a b) a)))

(= snd (pr)
  (pr (\ (a b) b)))

(= pair-map (f pr)
  (pr (\ (a b) (pair a (f b)))))

(= pair-1 (pair 1 2))

(= maybe (m r o) (m r o))

(= just (x) (\ (j n) (j x)))
(= none ()  (\ (j n) n))

(= maybe-map (f mb)
  (maybe mb (\ (x) (just (f x))) none))

(= j1 (just 1))

(= foldr (xs c n) (xs c n))

(= cons (x xs) (\ (c n) (c x (foldr xs c n))))
(= nil  ()     (\ (k t) t))

(= lst-1 (cons 2 (cons 1 (nil))))

(= sum (xs) (foldr xs (\ (y ys) (+ y (ys))) 0))

(= add (a b) (+ a b))

