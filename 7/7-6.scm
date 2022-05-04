; Exercise 7.6
(define count-elements
  (lambda (counts? lst)

    (define iter
      (lambda (acc rem)
        (cond ((null? rem) acc)
              ((counts? (car rem)) (iter (+ 1 acc) (cdr rem)))
              (else (iter acc (cdr rem))))))

    (iter 0 lst)))

(define lst '(1 2 3 4 5 6 7 8 9 10))
(count-elements (lambda (n) #t) lst)
(count-elements even? lst)
                           