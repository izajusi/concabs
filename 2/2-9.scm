(define occurrence
  (lambda (n d)
    (cond ((< n 0) (occurrence (- n) d))
          ((< n 10) (if (= n d) 1 0))
          (else (+ (if (= (remainder n 10) d) 1 0)
                   (occurrence (quotient n 10) d))))))

(occurrence 1123111 1)
(occurrence -666 6)