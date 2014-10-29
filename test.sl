; Basic untyped list definition and some functions
(= cons (x y)
  (\ (c n) (c x y)))

(= nil ()
  (\ (c n) (n)))

(= car (xs)
  (xs (\ (y ys) y)))

(= cdr (xs)
  (xs (\ (y ys) ys)))

(= foldr (xs a b)
  (xs (\ (y ys) (a y (foldr ys a b)))
      nil))

(= sum (xs)
  (foldr xs (\ (x y) (+ x y)) 0))

; this one is faulty
(= map (f xs)
  (foldr xs
    (\ (y ys) (cons (f y) ys))
    nil))

(= length (xs)
  (foldr xs (\ (y ys) (+ 1 ys)) 0))

(= square (x) (* x x))

(let
  ((lst1 (cons 3 (cons 2 (cons 1 (nil))))))
  (print (car (cdr (map square lst1)))))

