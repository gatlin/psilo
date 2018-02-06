; (defun id (x) x)
;(: three Float)
(def three 3.0)
(defun square (x) (* x x))
(defun times-2 (x) (* x 2))

;(def six (times-2 three))
;(: nine Float)
(def nine (square three))

(def eighty-one (square nine))

;(def eighteen-1 (times-2 nine)) ; Num a => a
;(def eighteen-2 (* six 3.0))    ; Float
;(def eighteen-3 (* six three))  ; Float
;(def eighteen-4 (* 2 nine))     ; Num a => a
;(def eighteen-5 (* 2.0 nine))   ; Float