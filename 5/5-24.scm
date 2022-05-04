; Exercise 5.24
(define positive-integer-upto-where-smallest
  (lambda (n f) ; return an integer i such that
    ; 1 <= i <= n and for all integers j
    ; in that same range, f(i) <= f(j)
    (define loop
      (lambda (where-smallest-so-far next-to-try)
        (if (> next-to-try n)
            where-smallest-so-far
            (loop (if (< (f next-to-try)
                         (f where-smallest-so-far))
                      next-to-try
                      where-smallest-so-far)
                  (+ next-to-try 1)))))
    (loop 1 2)))
