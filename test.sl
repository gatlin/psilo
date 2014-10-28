(= cons (\ (x y)
  (\ (f g) (f x y))))

(= nil (\ ()
  (\ (f g) (g))))

(= car (\ (xs)
  (xs (\ (x y) x))))

(= cdr (\ (xs)
  (xs (\ (x y) y))))

(= length (\ (xs)
  (let
    ((length-h (\ (xs n)
       (xs (\ (y ys) (length-h ys (+ n 1)))
           (\ ()     n)))))
    (length-h xs 0))))

(= square (\ (x) (* x x)))

(let
  ((list-1 (cons 1 (cons 2 (cons 3 (nil))))))
  (print (square (length list-1))))
