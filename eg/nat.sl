/*

The goal is for these to be equivalent (typechecking notwithstanding):

(data Nat ()
  (Zero)
  (Succ Nat))

(:: add : Nat -> Nat -> Nat)
(fn add ((Zero) n) n)
(fn add ((Succ m) n)
  (Succ (add m n)))

*/

(fn Zero ()  (fn (f g) (f)))
(fn Succ (n) (fn (f g) (g n)))
(fn add (mz n)
  (mz n
      (fn (m)
        (Succ (add m n)))))
