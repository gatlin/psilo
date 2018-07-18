(= id (x) x)

(: square (=> ((Num n)) (-> n n)))
(= square (x) (* x x))

(::= Constant (a b)
  (forall (r) (-> (-> a r) r)))

(= constant (x) (Constant (\ (k) (k x))))
(= get-constant (c) ((~Constant c) id))

(: map-constant (-> (-> a b) (Constant r a) (Constant r b)))
(= map-constant (fn c) c)

(= even-number? (n) (=? 0 (modulo n 2)))

(: c1 (Constant Int Int))
(= c1 (constant 2))
(= c2 (map-constant even-number? c1))
(= c3 (get-constant c2))
