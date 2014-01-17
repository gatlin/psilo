/* Y combinator */


(fn Y (f)
  ((fn (y)
     (f (y y)))
   (fn (y)
     (f (y y)))))
