; -- Game state ADT
(define make-game-state
  (lambda (n m) (cons n m)))

(define size-of-pile
  (lambda (game-state pile-number)
    (if (= pile-number 1)
        (car game-state)
        (cdr game-state))))

(define remove-coins-from-pile
  (lambda (game-state num-coins pile-number)
    (if (= pile-number 1)
        (make-game-state (- (size-of-pile game-state 1)
                            num-coins)
                         (size-of-pile game-state 2))
        (make-game-state (size-of-pile game-state 1)
                         (- (size-of-pile game-state 2)
                            num-coins)))))

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
    (newline)))

(define total-size
  (lambda (game-state)
    (+ (size-of-pile game-state 1)
       (size-of-pile game-state 2))))

; -- Move instruction ADT
(define make-move-instruction
  (lambda (coins pile)
    (cons pile coins)))

(define move-from-pile
  (lambda (move-instruction)
    (car move-instruction)))

(define move-coins-count
  (lambda (move-instruction)
    (cdr move-instruction)))

(define next-game-state
  (lambda (game-state move-instruction)
    (remove-coins-from-pile
     game-state
     (move-coins-count move-instruction)
     (move-from-pile move-instruction))))

; -- Computer strategies
(define simple-strategy
  (lambda (game-state)
    (if (> (size-of-pile game-state 1) 0)
        (make-move-instruction 1 1)
        (make-move-instruction 1 2))))

; Exercise 6.15
(define take-all-of-first-nonempty
  (lambda (game-state)
    (let ((pile-size-1 (size-of-pile game-state 1)))
      (if (> pile-size-1 1 0)
          (make-move-instruction pile-size-1 1)
          (make-move-instruction (size-of-pile game-state 2) 2)))))

; Exercise 6.16
(define take-one-from-random-pile
  (lambda (game-state)
    (let ((pile-to-take-from
           (cond ((= (size-of-pile game-state 1) 0) 2) ; Always take from pile 2 when pile 1 is empty
                 ((= (size-of-pile game-state 2) 0) 1) ; Always take from pile 1 when pile 2 is empty
                 (else (+ (random 2) 1)))))
      (make-move-instruction 1 pile-to-take-from))))

; Exercise 6.17
(define take-random-from-random-pile
  (lambda (game-state)
    (let ((pile-to-take-from
           (cond ((= (size-of-pile game-state 1) 0) 2) ; Always take from pile 2 when pile 1 is empty
                 ((= (size-of-pile game-state 2) 0) 1) ; Always take from pile 1 when pile 2 is empty
                 (else (+ (random 2) 1)))))
      (make-move-instruction
       (+ (random (size-of-pile game-state pile-to-take-from)) 1)
       pile-to-take-from))))

; Exercise 6.18
(define winning-strategy
  (lambda (game-state)
    (let ((pile-size-1 (size-of-pile game-state 1))
          (pile-size-2 (size-of-pile game-state 2)))
      (let ((pile-to-take-from
             (if (> pile-size-1 pile-size-2) 1 2))
            (coins-to-take
             (abs (- pile-size-1 pile-size-2))))
        (make-move-instruction
         (if (= coins-to-take 0) 1 coins-to-take)
         pile-to-take-from)))))

; Exercise 6.19
(define random-strategy
  (lambda (strat-a strat-b)
      (lambda (game-state)
        (let ((strat-to-execute (if (= (random 2) 0) strat-a strat-b)))
          (strat-to-execute game-state)))))

; -- Game methods
(define play-with-turns
  (lambda (game-state player strategy)
    (display-game-state game-state)
    (cond ((over? game-state)
           (announce-winner player))
          ((equal? player 'human)
           (play-with-turns (human-move game-state) 'computer strategy))
          ((equal? player 'computer)
           (play-with-turns (computer-move game-state strategy) 'human strategy))
          (else
           (error "player wasn’t human or computer:" player)))))

(define computer-move
  (lambda (game-state strategy)
    (let ((move-instruction (strategy game-state)))
      (display "I take ")
      (display (move-coins-count move-instruction))
      (display " coins from pile ")
      (display (move-from-pile move-instruction))
      (newline)
      (next-game-state game-state move-instruction))))

(define prompt
  (lambda (prompt-string)
    (newline)
    (display prompt-string)
    (newline)
    (read)))

(define human-move
  (lambda (game-state)
    (let ((p (prompt "Which pile will you remove from?")))
      (let ((n (prompt "How many coins do you want to remove?")))
        (remove-coins-from-pile game-state n p)))))

(define over?
  (lambda (game-state)
    (= (total-size game-state) 0)))

(define announce-winner
  (lambda (player)
    (if (equal? player 'human)
        (display "You lose. Better luck next time.")
        (display "You win. Congratulations."))))

; -- Game run
; (play-with-turns (make-game-state 15 20) 'human (random-strategy winning-strategy take-random-from-random-pile))


; -- Exercise 6.25
(define make-game-state-comparator
 (lambda (operator)
   (lambda (gs-a gs-b)
     (operator (total-size gs-a) (total-size gs-b)))))

(define game-state-< (make-game-state-comparator <))
(game-state-< (make-game-state 3 7) (make-game-state 1 12))

(define game-state-> (make-game-state-comparator >))
(game-state-> (make-game-state 3 7) (make-game-state 1 12))

(game-state-> (make-game-state 13 7) (make-game-state 1 12))