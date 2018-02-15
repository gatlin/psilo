(defun k (x y) y)
(defun f1 (f) (+ 3.0 (f 1.0 2.0)))

(def wut (f1 k))

;(defun square (x) (* x x))
;(defun times-2 (x) (* x 2))
;(defun times-2-and-square (x) (square (times-2 x)))

;(def three 3.0)
;(def six (times-2 three))
;(def nine (square three))
;(def eighty-one (square nine))
;(def thirty-six (times-2-and-square three))


;(def bad (* 3.0 #t)) ; doesn't work, which is expected
