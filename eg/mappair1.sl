/*

A map-pair ("zip with") function.

Ultimately these definitions should be equivalent (typechecking aside):

; assume list.sl is loaded as well

(:: mappair1 : (a -> b -> c) -> List a -> List b -> List c)
(fn mappair1 (f (Nil) zs)          (Nil))
(fn mappair1 (f (Cons x xs) (Nil)) (Nil))
(fn mappair1 (f (Cons x xs) (Cons y ys))
  (Cons (f x y) (mappair1 f xs ys)))

*/

(fn mappair1 (f #:List-1 #:List-2)
  (#:List-1
    (Nil)
    (fn (x xs)
      (#:List-2
        (Nil)
        (fn (y ys)
          (Cons (f x y) (mappair1 f xs ys)))))))
