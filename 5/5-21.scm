; Exercise 5.21
(define make-generator
  (lambda (f)
    (lambda (factor)
      (lambda (x)
        (f x factor)))))

(define make-multiplier (make-generator *))
(define times-5 (make-multiplier 5))
(times-5 10)

(define make-exponentiator (make-generator expt))
(define square (make-exponentiator 2))
(square 10)