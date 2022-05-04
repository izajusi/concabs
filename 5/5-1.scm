(define together-copies-of
  (lambda (combine quantity thing)
    (if (= quantity 1)
        thing
        (combine (together-copies-of combine
                                     (- quantity 1)
                                     thing)
                 thing))))

(define power
  (lambda (base exponent)
    (together-copies-of * exponent base)))

(define f-linear
  (lambda (combine acc quantity thing)
    (if (= quantity 1)
        acc
        (f-linear combine (combine acc thing) (- quantity 1) thing))))

(f-linear * 2 10 2)

(define f-logarithmic
  (lambda (combine quantity thing)
    (cond ((= quantity 1) thing)
          ((odd? quantity) (combine thing
                                    (f-logarithmic combine (- quantity 1) thing)))
          (else (let ((combo (f-logarithmic combine (/ quantity 2) thing)))
                  (combine combo combo))))))

(f-logarithmic + 1000000000 2)