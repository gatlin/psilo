(= Y
  (\ (target)
    ((\ (f)
      (target (\ (arg) ((f f) arg))))
     (\ (f)
      (target (\ (arg) ((f f) arg)))))))

(let
  ((fact-sort-of (\ (k)
     (\ (n)
       (if (=? n 0)
           1
           (* n (k (- n 1))))))))
  (let
    ((fact2 (Y fact-sort-of)))
    (print (fact2 5))))
