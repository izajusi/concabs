; Exercise 6.4
(define make-game-state
  ;; assumes no more than 99 coins per pile
  (lambda (n m) (+ (* 100 n) m)))

(define size-of-pile
  (lambda (game-state pile-number)
    (if (= pile-number 1)
        (quotient game-state 100)
        (remainder game-state 100))))

; Exercise 6.5
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