
(= List (\ (f) f))
(= foldr (\ (xs c n)
  ((xs c) n)))

(= cons (\ (x xs)
  (List (\ (c n)
    (c x (foldr xs c (n)))))))

(= nil (\ ()
  (List (\ (c n) (n)))))

(= car (\ (xs)
  (foldr xs (\ (y ys) y) nil)))

(let
  ((lst1 (cons 3 (cons 2 (cons 1 (nil))))))
  (print (car lst1)))
