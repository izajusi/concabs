(define num-digits-in-satisfying
  (lambda (n test?)
    (cond ((< n 0)
           (num-digits-in-satisfying (- n) test?))
          ((< n 10)
           (if (test? n) 1 0))
          ((test? (remainder n 10))
           (+ (num-digits-in-satisfying (quotient n 10) test?)
              1))
          (else
           (num-digits-in-satisfying (quotient n 10) test?)))))

; Exercise 5.4
(define num-digits
  (lambda (n)
    (num-digits-in-satisfying n (lambda (n) true))))

; Exercise 5.5
(define f-linear
  (lambda (acc n test?)
    (cond ((< n 0)
           (f-linear acc (- n) test?))
          ((< n 10)
           (+ acc (if (test? n) 1 0)))
          ((test? (remainder n 10))
           (f-linear (+ acc 1) (quotient n 10) test?))
          (else
           (f-linear acc (quotient n 10) test?)))))

; Exercise 5.6
(define sum-f-consecutive
  (lambda (acc low high f)
    (if (> low high)
        acc
        (sum-f-consecutive (+ acc (f low)) (+ low 1) high f))))

(define sum-of-squares
  (lambda (low high)
    (sum-f-consecutive 0 low high (lambda (n) (* n n)))))

(define sum-of-sqrt
  (lambda (low high)
    (sum-f-consecutive 0 low high sqrt)))
