;;;
; This test file highlights a really subtle and really annoying problem
; Environments are screwed up depending on whether a value is defined locally
; or globally.

; A simple wrapper around values
(= Box (b) b)
(= box (x) (Box (\ (f) (f x))))
(= unbox (bx)
  (bx (\ (x) x)))

(= b1 (Box 1))           ; Note which function is used here ...

(let ((b2 (box 2)))      ; ... versus here
  (let ((x (unbox b1))   ; the values are unboxed the same way
        (y (unbox b2)))
    (print (+ x y))))
