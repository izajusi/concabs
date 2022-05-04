; Exercise 5.19
(define increasing-on-integer-range?
  (lambda (f l h)
    (if (= l h)
        #t
        (let ((now (f l))
              (next (f (+ 1 l))))
          (if (>= now next)
              #f
              (increasing-on-integer-range? f (+ 1 l) h))))))