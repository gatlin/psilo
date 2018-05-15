(: square (=> ((Num n)) (-> n n)))
(= square (x) (* x x))

(= three 3)
(= six (* 2.0 three))
(= nine (square three))
(= thirty-six (square six))

(= five-1 5)
(= five-2 () 5)
(= ten-1 (* 2 five-1))
(= ten-2 (* 2 five-2))
