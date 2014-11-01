(= Box (x)
  (\ (f) (f x)))

(= unbox (bx)
  (bx (\ (x) x)))

(= map-box (f bx)
  (Box (f (unbox bx))))

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
  (unpair p (\ (a b) (pair a (f b)))))

(= add1 (x) (+ 1 x))

(let
  ((p1 (pair 10 20))
   (b1 (Box 5)))
  (print (unbox (map-box add1 b1))))
