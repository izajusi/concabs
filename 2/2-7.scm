(define sum-integers-from-to
  (lambda (low high)
    (if (> low high)
        0
        (+ (sum-integers-from-to low (- high 1))
           high))))

(define f
  (lambda (low high)
    (if (> low high)
        0
        (+ (sum-integers-from-to (+ low 1) high)
           low))))

(define assert
 (lambda (low high)
   (= (sum-integers-from-to low high)
      (f low high))))

(assert 4 9)
(assert 10 15)