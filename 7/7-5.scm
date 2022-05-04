; Exercise 7.5
(define combine
  (lambda (init combinator lst)

    (define iter
      (lambda (acc rem)
        (if (null? rem)
            acc
            (iter (combinator acc (car rem)) (cdr rem)))))

    (iter init lst)))

(define make-combine
  (lambda (init combinator)
    (lambda (lst)
      (combine init combinator lst))))

(define sum (make-combine 0 +))
(sum '(1 2 3 4 5))

(define product (make-combine 1 *))
(product '(1 2 3 4 5))