(define subtract-the-first
  (lambda (n)
    (if (= n 0)
        0
        (- n
           (subtract-the-first (- n 1))))))

(define f
  (lambda (n)
    (if (= (modulo n 2) 0)
        (/ n 2)
        (/ (+ n 1) 2))))

(define assert
  (lambda (n)
    (= (subtract-the-first n) (f n))))

(assert 0)
(assert 1)
(assert 2)
(assert 3)
(assert 4)
(assert 5)
(assert 6)
(assert 7)
(assert 8)