(def id (\ (x) x))
(def compose (\ (f g) (\ (x) (f (g x)))))

(def Box (\ (x) x))
(def box (\ (x) (Box (\ (f) (f x)))))
(def unbox (\ (bx) (bx id)))
(def box-map (\ (f bx) (box (f (unbox bx)))))

(def Const (\ (x) x))
(def const (\ (x) (Const (\ (f) (f x)))))
(def get-const (\ (c) (c id)))
(def const-map (\ (f c) (const (get-const))))

(def over (\ (l f s)
  (unbox ((l (compose box f) s) box-map))))

(def view (\ (l s)
  (get-const ((l const s) const-map))))

(def Pair (\ (p) p))
(def make-pair (\ (x y) (Pair (\ (f) (f x y)))))

(def fst (\ (f p)
  (\ (mapper)
    (p (\ (x y)
      (mapper (\ (new-x) (make-pair new-x y))
              (f x)))))))

(def snd (\ (f p)
  (\ (mapper)
    (p (\ (x y)
      (mapper (\ (new-y) (make-pair x new-y))
              (f y)))))))

(def p1 (make-pair 1 2))

(def +1 (\ (x) (+ 1 x)))

(def main (\ () (+1 2)))