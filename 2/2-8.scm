(define sum-of-squares
  (lambda (n)
    (if (= n 0)
        0
        (+ (sum-of-squares (- n 1))
           (* n n)))))

(= (sum-of-squares 1) 1)
(= (sum-of-squares 2) 5)
(= (sum-of-squares 3) 14)

(define sum-of-cubes
  (lambda (n)
    (if (= n 0)
        0
        (+ (sum-of-cubes (- n 1))
           (* n n n)))))

(= (sum-of-cubes 1) 1)
(= (sum-of-cubes 2) 9)
(= (sum-of-cubes 3) 36)

(define power
  (lambda (n p)
    (if (= p 0)
        1
        (* (power n (- p 1)) n))))

(define sum-of-powers
  (lambda (n p)
    (if (= n 0)
        0
        (+ (sum-of-powers (- n 1) p)
           (power n p)))))

(= (sum-of-powers 3 2) 14)
(= (sum-of-powers 3 3) 36)