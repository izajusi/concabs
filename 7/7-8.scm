(define lst '(1 2 3 4 5 6 7 8 9 10))

; Exercise 7.8.a
(define in-lst?
  (lambda (lst elem)

    (define iter
      (lambda (rem)
        (cond ((null? rem) #f)
              ((= elem (car rem)) #t)
              (else (iter (cdr rem))))))

    (iter lst)))

; Exercise 7.8.b
(define elem-satisfies?
  (lambda (lst predicate)

    (define iter
      (lambda (rem)
        (cond ((null? rem) #f)
              ((predicate (car rem)) #t)
              (else (iter (cdr rem))))))

    (iter lst)))

; Exercise 7.8.c
(define first-elem-satisfies
  (lambda (lst predicate)

    (define iter
      (lambda (rem)
        (cond ((null? rem) #f)
              ((predicate (car rem)) (car rem))
              (else (iter (cdr rem))))))

    (iter lst)))

; Exercise 7.8.d
(define all-elem-satisfies?
  (lambda (lst predicate)

    (define iter
      (lambda (rem)
        (cond ((null? rem) #t)
              ((not (predicate (car rem))) #f)
              (else (iter (cdr rem))))))

    (iter lst)))

; Exercise 7.8.e
(define first-elem-satisfies-at?
  (lambda (lst predicate)

    (define iter
      (lambda (idx rem)
        (cond ((null? rem) #f)
              ((predicate (car rem)) idx)
              (else (iter (+ idx 1) (cdr rem))))))

    (iter 0 lst)))

; Exercise 7.8.f
(define largest-elem
  (lambda (lst)

    (define iter
      (lambda (largest-yet rem)
        (cond ((null? rem) largest-yet)
              ((< largest-yet (car rem)) (iter (car rem) (cdr rem)))
              (else (iter largest-yet (cdr rem))))))

    (iter 0 lst)))

; Exercise 7.8.g
(define pos-largest-elem
  (lambda (lst)

    (define iter
      (lambda (idx idx-largest-yet largest-yet rem)
        (cond ((null? rem) idx-largest-yet)
              ((< largest-yet (car rem)) (iter (+ idx 1) idx (car rem) (cdr rem)))
              (else (iter (+ idx 1) idx-largest-yet largest-yet (cdr rem))))))

    (iter 0 0 0 lst)))

(pos-largest-elem lst)
(pos-largest-elem '(10 20 30 40 50 1 2 3))