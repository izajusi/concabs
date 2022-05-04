(define perfect?
  (lambda (n)
    (= (sum-of-divisors n) (* 2 n))))

(define sum-of-divisors
  (lambda (n)
    (define sum-from-plus ; sum of all divisors of n which are
      (lambda (low addend) ; >= low, plus addend
        (if (> low n)
            addend
            ; no divisors of n are greater than n
            (sum-from-plus (+ low 1)
                           (if (divides? low n)
                               (+ addend low)
                               addend)))))
    (sum-from-plus 1 0)))

(define sum-of-divisors-v2
  (lambda (n)
    (define sum-from-plus  ; sum of all divisors of n which are >= low
      (lambda (low addend) ;  and < low^2, plus addend.
        (cond ((> (square low) n) addend)
              ((= (square low) n) (sum-from-plus (+ low 1) (+ addend low)))
              ((divides? low n) (sum-from-plus (+ low 1) (+ addend low (/ n low))))
              (else (sum-from-plus (+ low 1) addend)))))

    (define square (lambda (n) (expt n 2)))

  (sum-from-plus 1 0)))

(define perfect?-v2
  (lambda (n)
    (= (sum-of-divisors-v2 n) (* 2 n))))

(define divides?
  (lambda (a b)
    (= (remainder b a) 0)))