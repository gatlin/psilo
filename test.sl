(: square (=> ((Num n)) (-> n n)))
(= square (x) (* x x))

(= five-1 5)
(= five-2 () 5)
(= ten-1 (* 2 five-1))
(= ten-2 (* 2 five-2))