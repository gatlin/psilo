/*

The goal is for these to be equivalent (typechecking notwithstanding):

(data Nat ()
  (Zero)
  (Succ Nat))

(:: add : Nat -> Nat -> Nat)
(\ add ((Zero) n) n)
(\ add ((Succ m) n)
  (Succ (add m n)))

*/

(\ zero ()  (\ (f g) (f)))
(\ succ (n) (\ (f g) (g n)))
(\ add (mz n)
  (mz n
      (\ (m)
        (succ (add m n)))))
