; Exercise 5.23
(define make-averaged-procedure
  (lambda (f g)
    (lambda (x)
      (/ (+ (f x) (g x)) 2))))

(define double (lambda (x) (* x 2)))
(define square (lambda (x) (* x x)))

(define new-procedure
  (make-averaged-procedure double square))