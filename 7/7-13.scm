(define integers-from-to
  (lambda (low high)
    (define iter
      (lambda (high lst)
        (if (> low high)
            lst
            (iter (- high 1)
                  (cons high lst)))))
    (iter high '())))

; Exercise 7.13.a
(define first-perfect-squares
  (lambda (n)
    (map (lambda (x) (* x x)) (integers-from-to 1 n))))

; Exercise 7.13.b
(define first-even-integers
  (lambda (n)
    (map (lambda (x) (* x 2)) (integers-from-to 1 n))))

; Exercise 7.13.c
(define sevens
  (lambda (n)
    (map (lambda (x) 7) (integers-from-to 1 n))))

; Exercise 7.13.d
(define list-of-lists
  (lambda (lst)
    (map (lambda (x) (integers-from-to 1 x)) lst)))

; Exercise 7.14
(define my-map
  (lambda (f lst)
    (if (null? lst)
        '()
        (cons (f (car lst)) (my-map f (cdr lst))))))
