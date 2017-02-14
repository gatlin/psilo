; strictly speaking we don't have "ints" but "words"
(defun negate-16 (word)
    (add (comp word) 1))

(defun minus-16 (a b)
    (add a (negate-16 b)))

; tail-recursive factorial
(defun fact-rec (n prod)
  (if (lt n 2)
    prod
    (fact-rec (minus-16 n 1) (mul prod n))))

; kickstarter-function for factorial
(defun fact (n) (fact-rec n 1))

(defun main () (fact 5))