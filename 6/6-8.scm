(define make-game-state
  (lambda (n m)
    (lambda (x)
      (if (odd? x)
          n
          m))))

(define size-of-pile
  (lambda (game-state pile-number)
    (game-state pile-number)))

; From exercise 6.5
(define remove-coins-from-pile
  (lambda (game-state num-coins pile-number)
    (let ((pile-1 (size-of-pile game-state 1))
          (pile-2 (size-of-pile game-state 2)))

      (if (= pile-number 1)
          (make-game-state
           (- pile-1 (if (> num-coins pile-1) pile-1 num-coins))
           pile-2)
          (make-game-state
           pile-1
           (- pile-2 (if (> num-coins pile-2) pile-2 num-coins)))))))

; Exercise 6.8
(define gs (make-game-state 5 10))
(size-of-pile gs 1)
(size-of-pile gs 2)
(define gs (remove-coins-from-pile gs 3 1))
(define gs (remove-coins-from-pile gs 3 2))
(size-of-pile gs 1)
(size-of-pile gs 2)