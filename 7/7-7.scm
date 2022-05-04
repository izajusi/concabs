(define lst '(1 2 3 4 5 6 7 8 9 10))

; Exercise 7.7
(define list-at
  (lambda (lst at)

    (define iter
      (lambda (idx rem)
        (cond ((null? rem) #f)
              ((= idx at) (car rem))
              (else (iter (+ idx 1) (cdr rem))))))

    (iter 0 lst)))

(list-at lst 0)
(list-at lst 3)
(list-at lst 9)