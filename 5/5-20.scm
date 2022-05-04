; Exercise 5.20
(define f
  (lambda (m b)
    (lambda (x) (+ (* m x) b))))

(define g (f 3 2))