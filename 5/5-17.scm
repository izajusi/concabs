; Exercise 5.17
(define integer-in-range-where-smallest
  (lambda (f a b)
    (if (= a b)
        a
        (let ((smallest-place-after-a
               (integer-in-range-where-smallest f (+ a 1) b)))
          (if (< (f a) (f smallest-place-after-a))
              a
              smallest-place-after-a)))))

(integer-in-range-where-smallest
 (lambda (x)
   (- (* x x) (* 2 x)))
 0 4)