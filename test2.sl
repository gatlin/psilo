(defun fact-rec (n prod)
  (if (< n 2)
    prod
    (fact-rec (- n 1) (* prod n))))

(defun fact (n) (fact-rec n 1))

(defun main () (fact 5))
