; Exercise 7.16
(define coin-combos
  (lambda (change coins)
    (cond ((= change 0) 1)
          ((< change 0) 0)
          ((null? coins) 0)
          (else (+ (coin-combos (- change (car coins)) coins)
                   (coin-combos change (cdr coins)))))))

(define coin-types '(1 5 10 25))

(coin-combos 1 coin-types)
(coin-combos 5 coin-types)
(coin-combos 100 coin-types)