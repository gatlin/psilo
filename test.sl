(= id (x) x)

(= box (x)
  (\ (f) (f x)))

(= add1 (x) (+ 1 x))

(let
  ((b1 (box 1)))
  (print (b1 add1)))
