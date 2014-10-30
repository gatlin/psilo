; Tests / scratchpad

(= Box (x)
  (\ (f) (f x)))

(= unbox (b)
  (b (\ (x) x)))

(= map-box (f bx)
  (Box (f (unbox bx))))

; Pairs
(= unpair (p f)
  (p f))

(= Pair (f) f)

(= pair (x y)
  (Pair (\ (f) (f x y))))

(= fst (p)
  (unpair p (\ (a b) a)))

(= snd (p)
  (unpair p (\ (a b) b)))


