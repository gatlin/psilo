(def make-pair (\ (x y) (\ (f) (f x y))))

(def p1 (make-pair 1 2))

(def fst (\ (p) (p (\ (x y) x))))
(def snd (\ (p) (p (\ (x y) y))))

(def fact (\ (n)
    (if (= n 0)
        1
        (* n (fact (- n 1))))))

(def main (\ () (fact 5)))