(: three (=> ((Num x)) x))
(= three 3)

(: six (=> ((Num d)) d))
(= six (* 2 three))

(: square (=> ((Num n)) (-> n n)))
(= (square x) (* x x))

(= nine (square three))
