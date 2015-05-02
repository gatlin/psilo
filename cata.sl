(= id (x) x)

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

(= foldr (xs c n) (xs c n))

(= cons (x xs) (\ (c n) (c x (foldr xs c n))))
(= nil  ()     (\ (k t) t))

(= lst-1 (cons 3 (cons 2 (cons 1))))

(= add (a b) (+ a b))
