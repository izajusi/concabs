(define make-game-state
  (lambda (n m k) (cons k (cons n m))))

(define size-of-pile
  (lambda (game-state pile-number)
    (cond ((= pile-number 3)
           (car game-state))
          ((= pile-number 1)
           (car (cdr game-state)))
          (else ;pile-number must be 2
           (cdr (cdr game-state))))))

; Exercise 6.10
(define gs (make-game-state 3 5 10))
(size-of-pile gs 1)
(size-of-pile gs 2)
(size-of-pile gs 3)