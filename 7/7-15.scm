; Exercise 7.15
(define count-combos
  (lambda (prize-list amount)
    (cond ((= amount 0) 1)
          ((< amount 0) 0)
          ((null? prize-list) 0)
          (else
           (+ (count-combos prize-list (- amount (car prize-list)))
              (count-combos (cdr prize-list) amount))))))

; Exercise 7.16
(define generate-prize-list
  (lambda (list-of-pair)

    (define recur
      (lambda (acc rem)
        (if (null? rem) acc
            (recur
                (add-prize-list
                 (car (car rem))
                 (car (cdr (car rem)))
                 acc)
              (cdr rem)))))

    (define add-prize-list
      (lambda (cost cnt acc)
        (if (= 0 cnt) acc
            (add-prize-list cost (- cnt 1) (cons cost acc)))))

    (recur '() list-of-pair)))

; Exercise 7.18
(define count-combos-non-exhaustive
  (lambda (prize-list amount)
    (cond ((= amount 0) 1)
          ((< amount 0) 0)
          ((null? prize-list) 1)
          (else
           (+ (count-combos-non-exhaustive prize-list (- amount (car prize-list)))
              (count-combos-non-exhaustive (cdr prize-list) amount))))))

; Exercise 7.19
(define count-combos-exclusive
  (lambda (prize-list amount)
    (cond ((= amount 0) 1)
          ((< amount 0) 0)
          ((null? prize-list) 0)
          (else
           (+ (count-combos-exclusive (cdr prize-list) (- amount (car prize-list)))
              (count-combos-exclusive (cdr prize-list) amount))))))

; Exercise 7.20
(define count-combos-non-exhaustive-exclusive
  (lambda (prize-list amount)
    (cond ((= amount 0) 1)
          ((< amount 0) 0)
          ((null? prize-list) 1)
          (else
           (+ (count-combos-non-exhaustive-exclusive (cdr prize-list) (- amount (car prize-list)))
              (count-combos-non-exhaustive-exclusive (cdr prize-list) amount))))))

(define prize-list
  (generate-prize-list
   '((10 9) (9 3) (8 2) (7 4) (6 3) (5 4) (4 3) (3 3) (2 4) (1 2))))

(count-combos prize-list 2)
(count-combos-non-exhaustive prize-list 2)
(count-combos-exclusive prize-list 2)
(count-combos-non-exhaustive-exclusive prize-list 2)
                                          