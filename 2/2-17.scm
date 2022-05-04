(define presents-on-day
  (lambda (n)
    (if (= n 1)
        1
        (+ n (presents-on-day (- n 1))))))

(define presents-through-day
  (lambda (n)
    (if (= n 1)
        1
        (+ (presents-on-day n) (presents-through-day (- n 1))))))

(presents-through-day 1)
(presents-through-day 2)
(presents-through-day 3)
(presents-through-day 4)
(presents-through-day 5)