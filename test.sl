;; TODO
; the typeclass constraints appear to only be propagated "forward":
; ("box-1",(∀a. (Wut a) => a Float)),
; ("box-2",(∀a. (Functor a, Wut a) => a Boolean))
; ("result",Boolean),("unbox",(∀a. (Box a) -> a))
; All three should just be `Box a -> a`

; Counter point: box-1 is in fact polymorphic. Just require a type annotation?

(= id (x) x)
(= bottom (bottom))

(: add-int-test-1 Int)
(= add-int-test-1 (int-add 1 2))

; testing that classes and instances even parse

(:: Box (a) (forall (r) (-> (-> a r) r)))

(@: C (l t)
  (: ac (-> l t)))

(:: Mul (a))
(: * (=> ((C (Mul a) (-> a a a))) (-> a a a)))
(= * (ac bottom))

(= square (x) (* x x))

(= wut (square 2.0))
