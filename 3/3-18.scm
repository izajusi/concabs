(define power
  (lambda (b n)
    (define power-iter
      (lambda (a pow)
        (cond ((> pow 0) (power-iter (* a b) (- pow 1)))
              ((< pow 0) (power-iter (/ a b) (+ pow 1)))
              (else a))))

    (power-iter 1 n)))

(power 2 0)
(power 2 2)
(power 2 -2)