; strictly speaking we don't have "ints" but "words"
(defun negate-16 (word)
    (add 1 (comp word)))

(defun minus-16 (a b)
    (add a (negate-16 b)))

; tail-recursive factorial
(defun fact-rec (n prod)
  (if (lt n 2)
    prod
    (fact-rec (minus-16 n 1) (mul prod n))))

; kickstarter-function for factorial
(defun fact (n) (fact-rec n 1))

(defun add-1-if-even (n)
  (if (eq 0 (modulo n 2))
    (add 1 n)
    n))

(defun main () (fact 5))