/*

A map-pair ("zip with") function, alternate definition.

Ultimately these definitions should be equivalent (typechecking aside):

; assume list.sl is loaded as well

(:: mappair2 : (a -> b -> c) -> List a -> List b -> List c)
(fn mappair2 (f (Nil) zs)   (Nil))
(fn mappair2 (f as (Nil))   (Nil))
(fn mappair2 (f (Cons x xs) (Cons y ys))
  (Cons (f x y) (mappair2 f xs ys)))

*/

(fn mappair2 (f #:List-1 #:List-2)
  (#:List-1
    (Nil)
    (fn (#:v-1 #:v-2)
      (#:List-2 (Nil)
        (fn (#:w-1 #:w-2)
          (#:List-1
            '()
            (fn (x xs)
              (#:List-2
                '()
                (fn (y ys)
                  (Cons (f x y) (mappair2 f xs ys)))))))))))
