/*

Ultimately these two should be equivalent (type checking notwithstanding):

(data List (t)
  (Nil)
  (Cons t (List t)))

(:: list-length : List t -> Int)
(fn list-length ((Nil)) 0)
(fn list-length ((Cons x xs))
  (+ 1
     (list-length xs)))

*/

(fn Nil  ()     (fn (f g) (f)))
(fn Cons (x xs) (fn (f g) (g x xs)))

(fn list-length (xs)
  (xs 0
      (fn (x xs)
        (+ 1
          (list-length xs)))))
