;; psilo standard library

; identity function
(= id (x) x)

; a (probably over-complicated) fixpoint combinator
(= fix
  ((\ (t)
     (t (\ (f fx)
       (f (fx f fx)))))
   (\ (f) (\ (n) (f n f)))))

; never evaluate this; this is only useful in certain awful situations
; because it can be assigned any type, and is thus a useful dummy value
(= diverge (\ () (fix id)))

; pairs are tensor products
(= Pair (p) p)
(= unpair (pr f) (pr f))
(= pair (a b)
  (Pair (\ (f) (f a b))))

(= fst (pr) (unpair pr (\ (a b) a)))
(= snd (pr) (unpair pr (\ (a b) b)))

; mapping a function onto a pair just maps its second element, by convention
(= pair-map (f pr)
  (unpair pr (\ (a b) (pair a (f b)))))

(= dup (x) (pair x x))

; Optional values
(= Maybe (m) m)

; similar to an if, except the truth carries another value
(= maybe (m j n) (m j n))

(= just (x)
  (Maybe (\ (j n) (j x))))

(= none ()
  (Maybe (\ (j n) n)))

(= maybe-map (f mb)
  (maybe mb (\ (x) (just (f x))) none))

; Lazy lists
(= List (l) l)
(= foldr (xs c n) (xs c n))

; prepends an item to a list
(= cons (x xs)
  (List (\ (c n) (c x (foldr xs c n)))))

; the empty list
(= nil ()
  (List (\ (c n) n)))

; map a function over every element in a list
(= list-map (f xs)
  (foldr xs (\ (y ys) (cons (f y) ys)) (nil)))

; Safely deconstructs a (potentially infinite) list into its
; head and tail, returning them as a pair. The head is returned
; in a `Maybe`
(= split (xs)
  ((\ (f)
     (foldr xs f (pair none nil)))
   (\ (y ys)
     (pair (just y)
           (List (\ (c n)
             (maybe (fst ys)
                    (\ (x) (c x (foldr (snd ys) c n)))
                    n)))))))

; extract the first item from the list
(= car (xs)
  (maybe (fst (split xs)) id diverge))

; drop the first item from the list, returning the rest
(= cdr (xs)
  (snd (split xs)))

; length of the list
(= length (xs) (foldr xs (\ (y ys) (+ 1 ys)) 0))

; append two lists
(= append (xs ys) (foldr xs cons ys))

; a left-fold. Useful for conserving memory in certain streaming
; applications.
(= foldl (f a xs)
  ((foldr xs
     (\ (b g)
       (\ (x)
         (g (f x b))))
     (\ (x) x)) a))

(= filter (pred xs)
  (foldr xs (\ (y ys) (if (pred y) (cons y ys) ys)) nil))

(= zero? (x) (=? 0 x))

(= take
  (fix (\ (f)
    (\ (args) (unpair args (\ (n xs)
      (if (zero? n) (nil)
        (if (zero? (length xs)) (nil)
          (cons (car xs) (f (pair (- n 1) (cdr xs))))))))))))

;; some useful functions for testing

(= square (x) (* x x))

(= even? (x) (=? 0 (mod x 2)))

; the gold standard test of any good fixpoint combinator
(= fact
  (fix (\ (f)
    (\ (n)
      (if (=? n 0)
          1
          (* n (f (- n 1))))))))

(= p1 (pair 1 2))

(= sum (xs)
  (foldl (\ (acc n) (+ acc n)) 0 xs))

(= l1 (cons 1 (cons 2 (cons 3 (nil)))))
