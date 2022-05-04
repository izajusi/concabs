; Exercise 6.13.a
(define make-move-instruction
  (lambda (pile coins)
    (cons pile coins)))

(define move-from-pile
  (lambda (move-instruction)
    (car move-instruction)))

(define move-coins-count
  (lambda (move-instruction)
    (cdr move-instruction)))

; Exercise 6.13.c
(define next-game-state
  (lambda (game-state move-instruction)
    (remove-coins-from-pile
     game-state
     (move-coins-count move-instruction)
     (move-from-pile move-instruction))))

(define simple-strategy
  (lambda (game-state)
    (if (> (size-of-pile game-state 1) 0)
        (make-move-instruction 1 1)
        (make-move-instruction 1 2))))