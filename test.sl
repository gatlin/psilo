;; TODO
; the typeclass constraints appear to only be propagated "forward":
; ("box-1",(∀a. (Wut a) => a Float)),
; ("box-2",(∀a. (Functor a, Wut a) => a Boolean))
; ("result",Boolean),("unbox",(∀a. (Box a) -> a))
; All three should just be `Box a -> a`

(= id (x) x)

(= is-even? (x) (=? 0 (modulo x 2.0)))

(@: Wut (m)
  (: wut (-> a (m a))))

(:: Box (a) (forall (r) (-> (-> a r) r)))

(: unbox (-> (Box a) a))
(= unbox (bx) ((~Box bx) id))

(@= Wut (Box)
  (= wut (x) (Box (\ (k) (k x)))))

(= huh (unbox (wut 1.0)))

(@: Functor (f)
  (: map (-> (-> a b) (f a) (f b))))

(@= Functor (Box)
  (= map (f bx) (box (f (unbox bx)))))

(= box-1 (wut 1.0))
(= box-2 (map is-even? box-1))
(= result (unbox box-2))
