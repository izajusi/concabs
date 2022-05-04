(define sum-of-digits
  (lambda (n)
    (define sum-plus ;(sum of n’s digits) + addend
      (lambda (n addend)
        (if (= n 0)
            addend
            (sum-plus (quotient n 10)
                      (+ addend (remainder n 10))))))
    (sum-plus n 0)))

; Exercise 5.10
(define digits-sum-divisible-by
  (lambda (n divisor)
    (= 0 (quotient (sum-of-digits n) divisor))))

(define make-verifier
  (lambda (f m)

    (define fold-digits
      (lambda (i rem addend)
        (if (= rem 0)
            addend
            (fold-digits (+ i 1)
                         (quotient rem 10)
                         (+ addend (f i (remainder rem 10)))))))

    (define check
      (lambda (n)
        (= (remainder (fold-digits 1 n 0) m) 0)))

    check))

; Exercise 5.11
(define check-isbn (make-verifier * 11))

;Exercise 5.12
(define f-upc
  (lambda (i di)
    (if (odd? i) di (* 3 di))))

(define check-upc (make-verifier f-upc 10))

; Exercise 5.13
(define f-cc
  (lambda (i di)
    (cond ((odd? i) di)
          ((< di 5) (* 2 di))
          (else (+ (* 2 di) 1)))))

(define check-cc (make-verifier f-cc 10))

; Exercise 5.14
(define f-us-postal
  (lambda (i di)
    (if (= i 1) (- di) di)))

(define check-us-postal (make-verifier f-us-postal 9))

(check-us-postal 48007467977)
(check-us-postal 48077462766)