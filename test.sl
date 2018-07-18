(::= Functor (f)
  (forall (r)
    (->
      (-> (forall (a b)
        (-> (-> a b) (f a) (f b))) r) r)))

(: functor (forall (f) (-> (forall (a b) (-> (-> a b) (f a) (f b))) (Functor f))))
(= functor (map-fn) (Functor (\ (k) (k map-fn))))

(= map (fctor f x) ((~Functor fctor) (\ (fn) (fn f x))))

;(= even-number? (x) (=? 0 (modulo x 2)))

;(= id (x) x)
;(::= Box (a) (forall (r) (-> (-> a r) r)))
;(= box (x) (Box (\ (k) (k x))))
;(= unbox (b) ((~Box b) id))

;(: functor-box (Functor Box))
;(= functor-box
;  (functor (\ (fn b)
;    (box (fn (unbox b))))))

;(: b1 (Box Float))
;(= b1 (box 2.0))

;(: b2 (Box Boolean))
;(= b2 (map functor-box even-number? b1))
