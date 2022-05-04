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

(define remove-coins-from-pile
  (lambda (game-state num-coins pile-number)
    (cond ((= pile-number 1)
           (make-game-state (- (size-of-pile game-state 1) num-coins)
                            (size-of-pile game-state 2)
                            (size-of-pile game-state 3)))
          ((= pile-number 2)
           (make-game-state (size-of-pile game-state 1)
                            (- (size-of-pile game-state 2) num-coins)
                            (size-of-pile game-state 3)))
          (else
           (make-game-state (size-of-pile game-state 1)
                            (size-of-pile game-state 2)
                            (- (size-of-pile game-state 3) num-coins))))))

(define display-game-state
  (lambda (game-state)
    (newline)
    (newline)
    (display "Pile 1: ")
    (display (size-of-pile game-state 1))
    (newline)
    (display "Pile 2: ")
    (display (size-of-pile game-state 2))
    (newline)
    (display "Pile 3: ")
    (display (size-of-pile game-state 3))
    (newline)
    (newline)))

(define total-size
  (lambda (game-state)
    (+ (size-of-pile game-state 1)
       (size-of-pile game-state 2)
       (size-of-pile game-state 3))))

; Exercise 6.11
(define gs (make-game-state 3 5 10))
(define gs (remove-coins-from-pile gs 2 1))
(define gs (remove-coins-from-pile gs 2 2))
(define gs (remove-coins-from-pile gs 2 3))
(display-game-state gs)
(total-size gs)

; Exercise 6.12
(define computer-move
  (lambda (game-state)
    (let ((pile (cond ((> (size-of-pile game-state 1) 0) 1)
                      ((> (size-of-pile game-state 2) 0) 2)
                      ((> (size-of-pile game-state 3) 0) 3))))
      (display "I take 1 coin from pile ")
      (display pile)
      (newline)
      (remove-coins-from-pile game-state 1 pile))))

(define gs (make-game-state 1 1 1))
(define gs (computer-move gs))
(display-game-state gs)
(define gs (computer-move gs))
(display-game-state gs)
(define gs (computer-move gs))
(display-game-state gs)