(define combination
  (lambda (n k)
    (cond ((= k 0) 1)
          ((> k n) 0)
          (else (+ (combination (- n 1) (- k 1))
                   (combination (- n 1) k))))))

(combination 6 1)
(combination 6 2)
(combination 6 3)
(combination 6 4)
(combination 6 5)
(combination 6 6)