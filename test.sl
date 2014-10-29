(= square (x) (* x x))
(= add1   (x) (+ x 1))
(= add1ThenSquare (x) (square (add1 x)))

(print (add1ThenSquare 10))
