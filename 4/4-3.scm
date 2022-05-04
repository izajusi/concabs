(define mod-expt-steps
  (lambda (exponent)
    (if (= exponent 0)
        0
        (if (even? exponent)
            (+ 1 (mod-expt-steps (/ exponent 2)))
            (+ 1 (mod-expt-steps (- exponent 1)))))))