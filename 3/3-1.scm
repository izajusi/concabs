(define factorial-product
  (lambda (a b c) ; compute a * b up to c
    (if (= b c)
        (* a c)
        (factorial-product (* a b) (+ b 1) c))))

(define factorial
  (lambda (n)
    (cond ((= n 0) 1)
          ((= n 1) 1)
          (else (factorial-product 1 2 n)))))