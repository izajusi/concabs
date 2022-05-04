(define occurrence-odd
  (lambda (n)
    (cond ((< n 0) (occurrence-odd (- n)))
          ((< n 10) (if (odd? n) 1 0))
          (else (+ (if (odd? (remainder n 10)) 1 0)
                   (occurrence-odd (quotient n 10)))))))

(occurrence-odd 1123111)
(occurrence-odd -666)