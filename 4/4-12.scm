(define ways-to-factor
  (lambda (n)
    (define ways-to-factor-using-no-smaller-than
      (lambda (n m)
        (cond ((> (expt m 2) n) 0)
              ((> (remainder n m) 0) (ways-to-factor-using-no-smaller-than n (+ m 1)))
              (else (+ 1
                       (ways-to-factor-using-no-smaller-than (/ n m) m)
                       (ways-to-factor-using-no-smaller-than n (+ m 1)))))))

    (ways-to-factor-using-no-smaller-than n 2)
    ))