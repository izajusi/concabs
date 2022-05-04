(define integers-from-to
  (lambda (low high)
    (if (> low high)
        '()
        (cons low
              (integers-from-to (+ 1 low) high)))))

; Exercise 7.3
(define even-integers-from-to
  (lambda (low high)
    (cond ((> low high) '())
          ((odd? low) (even-integers-from-to (+ 1 low) high))
          (else (cons low (even-integers-from-to (+ 1 low) high))))))

; Exercise 7.4
(define integers-from-to
  (lambda (low high)
    (define iter
      (lambda (high lst)
        (if (> low high)
            lst
            (iter (- high 1)
                  (cons high lst)))))
    (iter high '())))

(integers-from-to 2 7)


