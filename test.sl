(defun id (x) x)
(defun compose (f g) (\ (x) (f (g x))))

(defun Box (x) x)
(defun box (x) (Box (\ (f) (f x))))
(defun unbox (bx) (bx id))
(defun box-map (f bx) (box (f (unbox bx))))

(defun Const (x) x)
(defun const (x) (Const (\ (f) (f x))))
(defun get-const (c) (c id))
(defun const-map (f c) (const (get-const)))

(defun over (l f s)
  (unbox ((l (compose box f) s) box-map)))

(defun view (l s)
  (get-const ((l const s) const-map)))

(defun Pair (p) p)
(defun make-pair (x y) (Pair (\ (f) (f x y))))

(defun fst (f p)
  (\ (mapper)
    (p (\ (x y)
      (mapper (\ (new-x) (make-pair new-x y))
              (f x))))))

(defun snd (f p)
  (\ (mapper)
    (p (\ (x y)
      (mapper (\ (new-y) (make-pair x new-y))
              (f y))))))

(def p1 (make-pair 1 2))

(defun fact (n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(defun main () (fact (+1 3)))

(defun +1 (x) (+ 1 x))