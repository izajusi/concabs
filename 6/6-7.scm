; Exercise 6.7
(define exponent-of-in
  (lambda (n m)
    (define loop
      (lambda (pow rem)
        (if (> (remainder rem n) 0)
            pow
            (loop (+ pow 1) (/ rem n)))))

    (loop 0 m)))
