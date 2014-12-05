;;;
; This test file highlights a really subtle and really annoying problem
; Environments are screwed up depending on whether a value is defined locally
; or globally.

; A simple wrapper around values
(= Box (b) b)
(= box (x) (Box (\ (f) (f x))))
(= unbox (bx)
  (bx (\ (x) x)))

(= map-box (f bx)
  (unbox bx (\ (x) (box (f x)))))

(= square (x) (* x x))

(let ((b1 (box 1)))
  (print (unbox b1)))
