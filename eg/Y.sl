/* Y combinator */

(fn (f)
  ((fn (x) (f (fn (y) ((x x) y))))
   (fn (x) (f (fn (y) ((x x) y))))))
