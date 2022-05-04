(define power
  (lambda (x exp)
    (if (= exp 0)
        1
        (* x (power x (- exp 1))))))

(= (power 2 0) (expt 2 0))
