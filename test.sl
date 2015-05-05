;;;
; This test file highlights a really subtle and really annoying problem
; Environments are screwed up depending on whether a value is defined locally
; or globally.

(= compose (f g) (\ (x) (f (g x))))

(= add1 (x) (+ 1 x))
(= square (x) (* x x))

(= wut (compose square add1))
