; Utility - pairs
(= unpair (p f)
  (p f))

(= Pair (f) f)

(= pair (x y)
  (Pair (\ (f) (f x y))))

(= fst (p)
  (unpair p (\ (a b) a)))

(= snd (p)
  (unpair p (\ (a b) b)))

; Example data type: a Person
(= Person (name age)
  (\ (f) (f name age)))

(= age (p)
  (p (\ (p-name p-age) (pair p-age (\ (n-age) (Person p-name n-age))))))

(= name (p)
  (p (\ (p-name p-age) (pair p-name (\ (n-name) (Person n-name p-age))))))

(= birthday (p)
  (let ((age-lens (age p)))
    (unpair age-lens (\ (a new-p)
      (new-p (+ 1 a))))))

(= g (Person 'gatlin 26))

(let
  ((g-older (birthday (g))))
  (print (fst (age g-older))))
