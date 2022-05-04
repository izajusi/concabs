; Exercise 5.7
(define make-exponentiater
  (lambda (exp)
    (lambda (x)
      (define f
        (lambda (acc e)
          (if (= e 0)
              acc
              (f (* acc x) (- e 1)))))
      (f 1 exp))))