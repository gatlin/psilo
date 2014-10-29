; General utilities
(= id (x) x)
(= promise (x) (\ () x))

; Pairs!
(= unpair (p f)
  ((p (\ (x y) (\ () (f x y))))))

(= pair (x y)
  (\ (f) (f x y)))

(= fst (p)
  (unpair p (\ (a b) a)))

(= snd (p)
  (unpair p (\ (a b) b)))

(= pair-map (f p)
  (unpair p (\ (a b) (pair a (f b)))))

; Option types!
(= maybe (o y n)
  ((o (\ (x) (\ () (y x)))
      (\ ()  (n)))))

(= just (x)
  (\ (j n) (j x)))

(= none ()
  (\ (j n) (n)))

(= opt-map (f o)
  (maybe o (\ (x) (just (f x))) none))

