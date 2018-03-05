;(: square (=> ((Num n)) (-> n n )))
;(= (square x) (* x x))
;(= (times-2 x) (* 2 x))

(= (whoa x) ((\ (y) (* 2 y)) x))

;(: three (=> ((Num n)) n))
;(= three 3.0)
;(= six (times-2 three))
;(= nine (square three))

;(= eighteen (times-2 nine))

;(= (compose f g)
;  (\ (x) (f (g x))))

;(= (times-2-and-square x) (square (times-2 x)))

;(= thirty-six (times-2-then-square three))

;(= (fact n) (fact-helper n 1))
;(= (fact-helper n a)
  (if (=? n 0)
    a
    (fact-helper (- n 1) (* n a))))
