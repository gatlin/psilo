(defun id (x) x)
(defun square (x) (* x x))
(defun times-2 (x) (* x 2))

(def three 3.0)
(def six (times-2 three))
(def nine (square three))

;; seems like in `times-2` instead of the 2 being specialized to Float, the
;; argument is being generalized.
(def eighteen-1 (times-2 nine)) ; Num a => a
(def eighteen-2 (* six 3.0))    ; Float
(def eighteen-3 (* six three))  ; Float
(def eighteen-4 (* 2 nine))     ; Num a => a
(def eighteen-5 (* 2.0 nine))   ; Float