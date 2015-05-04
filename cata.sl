(= id (x) x)

(= turing
  (\ (f)
    (\ (n)
      (f n f))))

(= fix
  (turing (\ (f fx)
    (f (fx f fx)))))

(= fact
  (fix (\ (f)
    (\ (n)
      (if (=? n 0)
          1
          (* n (f (- n 1))))))))

(= diverge (\ () (fix id)))

(= const (x)
  (\ (y) x))

(= square (x) (* x x))

(= Pair (p) p)
(= unpair (pr f) (pr f))
(= pair (a b)
  (Pair (\ (f) (f a b))))

(= fst (pr) (unpair pr (\ (a b) a)))
(= snd (pr) (unpair pr (\ (a b) b)))

(= pair-map (f pr)
  (unpair pr (\ (a b) (pair a (f b)))))

(= p1 (pair 1 2))

(= dup (x) (pair x x))

(= Maybe (m) m)
(= maybe (m j n) (m j n))

(= just (x)
  (Maybe (\ (j n) (j x))))

(= none ()
  (Maybe (\ (j n) n)))

(= maybe-map (f mb)
  (maybe mb (\ (x) (just (f x))) none))

(= List (l) l)
(= foldr (xs c n) (xs c n))

(= cons (x xs)
  (List (\ (c n) (c x (foldr xs c n)))))

(= nil ()
  (List (\ (c n) n)))

(= list-map (f xs)
  (foldr xs (\ (y ys) (cons (f y) ys)) (nil)))

(= l1 (cons 1 (cons 2 (cons 3 (nil)))))

(= split (xs)
  ((\ (f)
     (foldr xs f (pair none nil)))
   (\ (y ys)
     (pair (just y)
           (List (\ (c n)
             (maybe (fst ys)
                    (\ (x) (c x (foldr (snd ys) c n)))
                    n)))))))

(= car (xs)
  (maybe (fst (split xs)) id diverge))

(= cdr (xs)
  (snd (split xs)))

(= length (xs) (foldr xs (\ (y ys) (+ 1 ys)) 0))

(= append (xs ys) (foldr xs cons ys))

(= foldl (f a xs)
  ((foldr xs
     (\ (b g)
       (\ (x)
         (g (f x b))))
     (\ (x) x)) a))

(= sum (xs)
  (foldl (\ (acc n) (+ acc n)) 0 xs))

