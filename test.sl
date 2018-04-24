(= (square x) (* x x))
(= three 3)
(= nine (square three))

(= (compose f g)
  (\ (x) (f (g x))))
