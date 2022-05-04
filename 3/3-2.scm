(define exp-2-iter
  (lambda (a b)
    (if (odd? b) a
        (exp-2-iter (+ a 1) (/ b 2)))))

(define exp-2
  (lambda (n)
    (exp-2-iter 0 n)))

(exp-2 8)
(exp-2 24)
(exp-2 1)
(exp-2 6)
