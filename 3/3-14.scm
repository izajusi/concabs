(define closest-power
  (lambda (b n)

    (define closest-power-iter
      (lambda (pow rem)
        (let ((next (quotient rem b)))
        (if (= next 0)
            pow
            (closest-power-iter (+ pow 1) next)))))

    (closest-power-iter 0 n)))
