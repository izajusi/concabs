(define exp-2
  (lambda (n)
    (if (odd? n) 0
        (+ (exp-2 (/ n 2)) 1))))

(exp-2 8)
(exp-2 24)
(exp-2 1)
(exp-2 6)