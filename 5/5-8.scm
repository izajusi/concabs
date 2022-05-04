(define make-repeated-version-of
  (lambda (f) ; make a repeated version of f
    (define the-repeated-version
      (lambda (b n) ; which does f n times to b
        (if (= n 0)
            b
            (the-repeated-version (f b) (- n 1)))))
    the-repeated-version))

; Exercise 5.8 and 5.9
(define make-consecutive-version-of
  (lambda (f-acc f-foreach)
    (define the-consecutive-version
      (lambda (acc low high)
        (if (> low high)
            acc
            (the-consecutive-version (f-acc acc (f-foreach low)) (+ low 1) high))))

    the-consecutive-version))

(define mult-acc (lambda (acc x) (* acc x)))
(define sum-acc (lambda (acc x) (+ acc x)))

(define nop-foreach (lambda (n) n))

(define factorial (make-consecutive-version-of mult-acc nop-foreach))
(define sum-of-first (make-consecutive-version-of sum-acc nop-foreach))
(define sum-of-squares (make-consecutive-version-of sum-acc (lambda (n) (* n n))))
(define sum-of-cubes (make-consecutive-version-of sum-acc (lambda (n) (* (* n n) n))))
