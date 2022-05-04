(define integers-from-to
  (lambda (low high)
    (define iter
      (lambda (high lst)
        (if (> low high)
            lst
            (iter (- high 1)
                  (cons high lst)))))
    (iter high '())))

(define first-elements-of
  (lambda (n list)
    (if (= n 0)
        '()
        (cons (car list)
              (first-elements-of (- n 1)
                                 (cdr list))))))

; Exercise 7.10
(define list-tail ; Returns all but the first n elements of lst.
  (lambda (lst n)

    (define recur
      (lambda (idx rem)
        (cond ((null? rem) '())
              ((< idx n) (recur (+ idx 1) (cdr rem)))
              (else (cons (car rem) (recur (+ idx 1) (cdr rem)))))))

    (recur 0 lst)))

(define interleave    ; interleaves lst1 and lst2, starting with
  (lambda (lst1 lst2) ; the first element of lst1 (if any)
    (if (null? lst1)
        lst2
        (cons (car lst1)
              (interleave lst2 (cdr lst1))))))

(define shuffle
  (lambda (deck size)
    (let ((half (quotient (+ size 1) 2)))
      (interleave (first-elements-of half deck)
                  (list-tail deck half)))))

(define multiple-shuffle
  (lambda (deck size times)
    (if (= times 0)
        deck
        (multiple-shuffle (shuffle deck size)
                          size (- times 1)))))

; Exercise 7.11.a
(define in-order? ; Assuming that list is not empty.
  (lambda (lst)
    (define iter
      (lambda (prev rem)
        (cond ((null? rem) #t)
              ((>= prev (car rem)) #f)
              (else (iter (car rem) (cdr rem))))))

    (iter (car lst) (cdr lst))))

; Exercise 7.11.b
(define shuffle-number
  (lambda (n)
    (define iter
      (lambda (shuffle-count prev-shuffle)
        (if (in-order? prev-shuffle)
            shuffle-count
            (iter (+ shuffle-count 1) (shuffle prev-shuffle n)))))

    (let ((deck (integers-from-to 1 n)))
      (iter 1 (shuffle deck n)))))
