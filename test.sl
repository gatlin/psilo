(= promise (x) (\ () x))
(= Box (b) b)
(= unbox (b)
  (b (\ (x) x)))

(= box (x)
  (Box (\ (f) (f x))))

(= map-box (f b)
  (box (f (unbox b))))

(= add1 (x) (+ 1 x))

(let
  ((b1 (box 1)))
  (let
    ((b2 (map-box add1 b1)))
    (print (unbox b2))))
