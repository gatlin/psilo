(: square (=> ((Num n)) (-> n n)))
(= square (x) (* x x))

(= three 3.0)
(= six (* 2 three))
(= nine (square three))
(= thirty-six (square six))

(= five-1 5.0)
(= five-2 () 5.0)
(= ten-1 (* 2 five-1))
(= ten-2 (* 2 five-2))

(: times-2-then-square (=> ((Num n)) (-> n n)))
(= times-2-then-square (x) (square (* 2 x)))
