(define improve
  (lambda (phi)
    (+ 1 (/ 1 phi))))

(define approximate-golden-ratio
  (lambda (tolerance)
    (define find-approximation-from
      (lambda (starting-point)
        (if (good-enough? starting-point)
            starting-point
            (find-approximation-from (improve starting-point)))))

    (define good-enough?
      (lambda (approximation)
        (< (/ 1 (square (denominator approximation)))
           tolerance)))

    (define square
      (lambda (n)
        (expt n 2)))

    (find-approximation-from 1)))