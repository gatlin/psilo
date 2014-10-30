; Utilities
(= promise (x) (\ () x))

; Pairs
(= unpair (p f)
  (p f))

(= Pair (f) f)

(= pair (x y)
  (Pair (\ (f) (f x y))))

(= fst (p)
  (unpair p (\ (a b) a)))

(= snd (p)
  (unpair p (\ (a b) b)))

; Options
(= maybe (o yes no)
   (o yes no))

(= Option (x) x)

(= just (x)
  (Option (\ (j n) (j x))))

(= none ()
  (Option (\ (j n) (n))))

; Lists
(= foldr (xs c n)
  (xs c n))

(= List (l) l)

(= cons (x xs)
  (List (\ (c n) (c x (foldr xs c n)))))

(= nil ()
  (List (\ (c n) (n))))

(= split (xs)
  (let ((f (\ (y ys)
             (pair (just y)
                   (List (\ (c n)
                     (maybe (fst ys)
                            (\ (x) (c x (foldr ((snd ys)) c n)))
                            n)))))))
  (foldr xs f (pair none nil))))

(= car (xs)
  (maybe (fst (split xs)) (\ (x) x) (promise -1)))

(= cdr (xs)
  (snd (split xs)))

(= sum (xs)
  (foldr xs (\ (y ys) (+ y ys)) (promise 0)))

(= map-list (f xs)
  (foldr xs (\ (y ys) (cons (f y) ys)) xs))

; Miscellaneous examples
(= add1 (x) (+ 1 x))
(= fact (x)
  (if (=? x 0)
      1
      (* x (fact (- x 1)))))

(let
  ((lst1 (cons 3 (cons 2 (cons 1 (nil))))))
  (print (fact (+ (sum lst1)
               (car (cdr lst1))))))
