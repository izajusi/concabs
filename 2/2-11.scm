(define sum-digit
  (lambda (n)
    (cond ((< n 0) (- (sum-digit (- n))))
          ((< n 10) n)
          (else (+ (remainder n 10)
                   (sum-digit (quotient n 10)))))))

(sum-digit 1123111)
(sum-digit -666)
(sum-digit 5551)