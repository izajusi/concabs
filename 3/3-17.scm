(define ffp
  (lambda (n k)

    (define ffp-iter
      (lambda (a next)
        (if (<= next (- n k))
            a
            (ffp-iter (* a next) (- next 1)))))

    (ffp-iter n (- n 1))))

(ffp 7 3)