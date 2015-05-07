;; psilo standard library

; identity function
(= id (x) x)

(= compose (f g) (\ (x) (f (g x))))

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
(= unpair (p f) (p f))
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

; Box, the identity functor
(= Box (b) b)
(= unbox (bx) (bx id))
(= box (x) (Box (\ (f) (f x))))

(= box-map (f bx)
  ((\ (x) (box (f x)))
   (unbox bx)))

; Const, similar to Box but it does not mutate its contents when mapped over
(= Const (c) c)
(= const (x) (Const (\ (f) (x))))
(= get-const (c) (c id))
(= const-map (f c) c)

; Lens utilities!
(= over (l f s)
  (unbox ((l (compose box f) s) box-map)))

(= view (l s)
  (get-const ((l Const s) const-map)))

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

(= list-nil? (xs)
  (((unpair (split xs) (\ (mh t)
    (maybe mh (\ (h) #f) #t))))))

; new and improved! as in, it works now!
(= take (n xs)
  (if (zero? n) (nil)
    (if (list-nil? xs) (nil)
      (cons (car xs) (take (- n 1) (cdr xs))))))

; very useful in combination with `take`
(= unfold (gen seed)
  ((\ (u) (u (pair gen seed)))
   (fix (\ (u) (\ (args) (unpair args (\ (gen seed)
     (cons seed (u (pair gen (gen seed)))))))))))

;; some useful functions for testing

(= square (x) (* x x))
(= even? (x) (=? 0 (mod x 2)))
(= add1 (x) (+ 1 x))

; the gold standard test of any good fixpoint combinator
(= fact
  (fix (\ (f)
    (\ (n)
      (if (=? n 0)
          1
          (* n (f (- n 1))))))))

(= fact-rec (n)
  (if (zero? n)
      1
      (* n (fact-rec (- n 1)))))

(= p1 (pair 1 2))

(= sum (xs)
  (foldr xs (\ (y ys) (+ y ys)) 0))

(= l1 (cons 1 (cons 2 (cons 3 (nil)))))

(= powers-of-2 (unfold square 2))

(= Person (p) p)
(= person (name age) (Person (\ (f) (f name age))))

(= name (f p)
  (\ (mapper)
    (p (\ (n a)
      (mapper (\ (new-n) (person new-n a))
              (f n))))))

(= age (f p)
  (\ (mapper)
    (p (\ (n a)
      (mapper (\ (new-a) (person n new-a))
              (f a))))))

(= george (person 'george-washington 283))

(= birthday (p)
  (over age add1 p))

(= nats (unfold add1 1))

