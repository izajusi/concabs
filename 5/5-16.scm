; Exercise 5.16
(define compose
  (lambda (f g)
    (lambda (n)
      (f (g n)))))

((compose sqrt abs) -4)