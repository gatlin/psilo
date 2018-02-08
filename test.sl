(defun square (x) (* x x))
(defun times-2 (x) (* x 2))
(defun times-2-and-square (x) (square (times-2 x)))
;(defun square-times-2-square (x) (square (times-2 (square x))))

(def three 3.0)
(def six (times-2 three))
(def nine (square three))
;(def eighty-one (square nine))
(def thirty-six (times-2-and-square three))

(def eighteen-1 (times-2 nine)) ; Num a => a
(def eighteen-2 (* six 3.0))    ; Float
;(def eighteen-3 (* six three))  ; Float
;(def eighteen-4 (* 2 nine))     ; Num a => a
;(def eighteen-5 (* 2.0 nine))   ; Float

;(defun compose (f g) (\ (x) (f (g x))))
;(def times-2-and-square (compose square times-2))
;(defun times-2-and-square (x) (square (times-2 x)))

;(: mult (-> Float Float Float))
;(defun mult (x y) (* x y))
;(def x (mult #t three))