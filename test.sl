(: three (=> ((Num x)) x))
(= three 3)

(: six (=> ((Num d)) d))
(= six (* 2 three))

(: square (=> ((Num n)) (-> n n)))
(= square (x) (* x x))

(= nine (square three))

(= five-1 5)
(= five-2 () 5)
(= ten-1 (* 2 five-1))
(= ten-2 (* 2 five-2))
(= twenty-five (square five-2))
