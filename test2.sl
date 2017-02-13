; comments are now operational!

; tail-recursive factorial
(defun fact-rec (n prod)
  (if (< n 2)
    prod
    (fact-rec (- n 1) (* prod n))))

; kickstarter-function for factorial
(defun fact (n) (fact-rec n 1))

(defun main () (fact 5))
