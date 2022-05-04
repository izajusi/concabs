;Exercise 7.9.a
(define list-<
  (lambda (l1 l2)

    (define iter
      (lambda (rem1 rem2)
        (cond ((null? rem1) (null? rem2))
              ((null? rem2) (null? rem1))
              ((not (< (car rem1) (car rem2))) #f)
              (else (iter (cdr rem1) (cdr rem2))))))

    (iter l1 l2)))

; Exercise 7.9.b
(define list-compare?
  (lambda (comparator l1 l2)

    (define iter
      (lambda (rem1 rem2)
        (cond ((null? rem1) (null? rem2))
              ((null? rem2) (null? rem1))
              ((not (comparator (car rem1) (car rem2))) #f)
              (else (iter (cdr rem1) (cdr rem2))))))

    (iter l1 l2)))

(define list-<
  (lambda (l1 l2)
    (list-compare? < l1 l2)))

(list-< '(1 2 3) '(3 4 5))
(list-< '(1 2 3) '(5 4 3))