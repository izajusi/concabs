(define square
  (lambda (x) (* x x)))

(define cube
  (lambda (x) (* x (* x x))))

(define reverse
  (lambda (lst)
    (define reverse-onto
      (lambda (lst1 lst2)
        (if (null? lst1)
            lst2
            (reverse-onto (cdr lst1)
                          (cons (car lst1)
                                lst2)))))
    (reverse-onto lst '())))

; Exercise 7.40
(define function-sum
  (lambda (lst)

    (lambda (x)
      (define iter
        (lambda (rem acc)
          (if (null? rem)
              acc
              (iter
               (cdr rem)
               (+ acc ((car rem) x))))))

      (iter lst 0))))

; Exercise 7.41
(define square-sum
  (lambda (lst)

    (define iter
      (lambda (rem acc)
        (if (null? rem)
            acc
            (iter (cdr rem) (+ acc (square (car rem)))))))

    (iter lst 0)))

; Exercise 7.42
(define apply-all
  (lambda (lst x)

    (define iter
      (lambda (rem acc)
        (if (null? rem)
            acc
            (iter
             (cdr rem)
             (cons ((car rem) x) acc)))))

    (reverse (iter lst '()))))

; Exercise 7.45
; Version 1: make-couple does the comparison
(define make-couple
  (lambda (a b)
    (if (> a b)
        (cons b a)
        (cons a b))))

(define smaller
  (lambda (couple)
    (car couple)))

(define larger
  (lambda (couple)
    (cdr couple)))

; Version 2: smaller/larger does the comparison
(define make-couple
  (lambda (a b)
    (cons a b)))

(define smaller
  (lambda (couple)
    (let ((a (car couple))
          (b (cdr couple)))
      (if (> a b) b a))))

(define larger
  (lambda (couple)
    (let ((a (car couple))
          (b (cdr couple)))
      (if (> a b) a b))))

; Exercise 7.46
(define make-list-scaler
  (lambda (x)

    (define iter
      (lambda (rem acc)
        (if (null? rem)
            acc
            (iter (cdr rem) (cons (* (car rem) x) acc)))))

    (define scale
      (lambda (lst)
        (reverse (iter lst '()))))

    scale))

; Exercise 7.47
(define map-2
  (lambda (op lst1 lst2)

    (define iter
      (lambda (rem1 rem2 acc)
        (cond ((null? rem1) acc)
              ((null? rem2) acc)
              (else
               (iter
                (cdr rem1)
                (cdr rem2)
                (cons (op (car rem1) (car rem2)) acc))))))

    (reverse (iter lst1 lst2 '()))))

; Exercise 7.49
(define all-are
  (lambda (predicate)

    (define iter
      (lambda (rem)
        (cond ((null? rem) #t)
              ((predicate (car rem)) (iter (cdr rem)))
              (else #f))))

    (lambda (lst)
      (iter lst))))

; Exercise 7.50
(define repeat
  (lambda (num times)

    (define iter
      (lambda (rem acc)
        (if (= 0 rem)
            acc
            (iter (- rem 1) (cons num acc)))))

    (iter times '())))

; Exercise 7.51
(define make-list-combiner
  (lambda (op)

    (define combiner
      (lambda (lst1 lst2)

        (define iter
          (lambda (rem1 rem2 acc)
            (cond ((null? rem1) acc)
                  ((null? rem2) acc)
                  (else (iter
                         (cdr rem1)
                         (cdr rem2)
                         (cons (op (car rem1) (car rem2)) acc))))))

        (reverse (iter lst1 lst2 '()))))

    combiner))
