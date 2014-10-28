(let
  ((cons (\ (x y)               ; Prepend an item to a list
           (\ (f g) (f x y))))
   (nil  (\ ()                  ; Create an empty list
           (\ (f g) (g))))
   (car  (\ (xs)                ; Return head of a list
           (xs (\ (x y) x))))
   (cdr  (\ (xs)                ; Return tail of a list
           (xs (\ (x y) y))))
   (length (\ (xs)              ; Calculate the length of a list
     (let
       ((length-helper (\ (xs n)
          (xs (\ (y ys) (length-helper ys (+ n 1)))
              (\ ()     n)))))
       (length-helper xs 0)))))
  (let
     ; Sample list of 3 numbers
    ((list-1 (cons 1 (cons 2 (cons 3 (nil))))))
    (length (cdr list-1))))
