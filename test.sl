(: wut (-> (-> (-> a b) a b) a))

;(defun id (x) x)

(: three Float)
;(def three (id 3.0))

(: times-2 (-> Float Float))
;(defun times-2 ((: x (=> ((Num a)) a))) (* x 2))

;(: eight Float)
;(def eight (times-2 4.0))

(: square (=> ((Num a)) (-> a a)))
;(defun square (x) (* x x))

;(def nine (square 3.0))
;(def four (square 2))

(: fact (=> ((Num a) (Ord a)) (-> a a)))
;(defun fact (n) (if (< n 2) n (fact (* n (- n 1)))))


;(defun compose (f g) (\ (x) (f (g x))))

;;;;; For later

;(defun Box (x) x)
;(defun box (x) (Box (\ (f) (f x))))
;(defun unbox (bx) (bx id))
;(defun box-map (f bx) (box (f (unbox bx))))

;(defun Const (x) x)
;(defun const (x) (Const (\ (f) (f x))))
;(defun get-const (c) (c id))
;(defun const-map (f c) (const (get-const)))

;(defun over (l f s)
  (unbox ((l (compose box f) s) box-map)))

;(defun view (l s)
;  (get-const ((l const s) const-map)))

;(defun Pair (p) p)
;(defun make-pair (x y) (Pair (\ (f) (f x y))))

;(defun fst (f p)
;  (\ (mapper)
;    (p (\ (x y)
;      (mapper (\ (new-x) (make-pair new-x y))
;              (f x))))))

;(defun snd (f p)
;  (\ (mapper)
;    (p (\ (x y)
;      (mapper (\ (new-y) (make-pair x new-y))
;              (f y))))))

;(def p1 (make-pair 1 2))

;(defun fact-no-tc (n)
;  (if (= n 1)
;    1
;    (* n (fact-no-tc (- n 1)))))

;(defun fact-tc-helper (product n)
;  (if (< n 2)
;    product
;    (fact-tc-helper (* product n)
;                    (- n 1))))

;(defun fact-tc (n) (fact-tc-helper 1 n))

;(defun main ()
;    ((\ (pr)
;       (+1 (view fst pr)))
;     (make-pair 1 2)))

;(defun +1 (x) (+ 1 x))