(= promise (x) (\ () x))
(= Box (b) b)
(= unbox (b)
  (b (\ (x) x)))

(= box (x)
  (Box (\ (f) (f x))))

(= map-box (f b)
  (box (f (unbox b))))

(= add1 (x) (+ 1 x))

(= a 1)

(let
  ((b 2))
  (print (+ a b)))
