; the compiler will eventually accept this program

(::= Nat ()
  (: Zero ())
  (: Succ Nat))

(: add (-> Nat Nat Nat))
(= add ('(Zero) n) n)
(= add ('(Succ m) n)
  `(Succ ,(add m n)))
