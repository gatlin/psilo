(def three (id 3.0))
(defun times-2 ((: x (=> ((Num a)) a))) (* x 2))
(defun square (x) (* x x))
(defun fact (n)
    (if (< n 2)
        n
        (fact (* n (- n 1)))))
(defun compose (f g) (\ (x) (f (g x))))