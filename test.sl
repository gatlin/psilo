(defun square (x) (* x x))
(defun times-2 (x) (* x 2))
(defun times-2-and-square (x) (square (times-2 x)))

(def three 3.0)
(def six (times-2 three))
(def nine (square three))
(def eighty-one (square nine))
(def thirty-six (times-2-and-square three))

;(def eighteen-1 (times-2 nine))
;(def eighteen-2 (* six 3.0))
;(def eighteen-3 (* six three))
;(def eighteen-4 (* 2 nine))
;(def eighteen-5 (* 2.0 nine))

;(defun compose (f g) (\ (x) (f (g x))))

;(def bad (* 4 #t))