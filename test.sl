;(defun id (x) x)

(: three Float)
(def three (id 3.0))

(: times-2 (-> Float Float))
(defun times-2 ((: x (=> ((Num a)) a))) (* x 2))

;(: eight Float)
;(def eight (times-2 4.0))

(: square (=> ((Num a)) (-> a a)))
(defun square (x) (* x x))

;(def nine (square 3.0))
;(def four (square 2))

(: fact (=> ((Num a) (Ord a)) (-> a a)))
(defun fact (n) (if (< n 2) n (fact (* n (- n 1)))))

(defun compose (f g) (\ (x) (f (g x))))