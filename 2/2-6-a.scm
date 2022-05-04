(define subtract-the-first
  (lambda (n)
    (if (= n 0)
        0
        (- (subtract-the-first (- n 1))
           n))))

(define f
  (lambda (n)
    (* (- n) (+ n 1) 1/2)))

(define assert
  (lambda (n)
    (= (subtract-the-first n) (f n))))

(subtract-the-first 9)
(assert 1)
(assert 2)
(assert 3)
(assert 4)
(assert 9)