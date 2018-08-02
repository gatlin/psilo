(= id (x) x)

(@: Functor (f)
  (: map (-> (-> a b) (f a) (f b))))


(@: Monad (=> ((Functor m)) (m))
  (: unit (-> a (m a)))

  (: join (-> (m (m a)) (m a)))

  (: bind (-> (m a) (-> a (m b)) (m b)))
  (= bind (m f) (join (map f m))))

(= square (x) (* x x))

(= factorial (n)
  (if (=? n 1)
      1
      (* n (factorial (- n 1)))))

(:: Box (a)
  (forall (r) (-> (-> a r) r)))

(: box (-> a (Box a)))
(= box (x) (Box (\ (k) (k x))))

(: unbox (-> (Box a) a))
(= unbox (bx) ((~Box bx) id))
