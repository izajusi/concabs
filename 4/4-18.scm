(define sum-half
  (lambda (a b)
    (let ((diff (- b a)))
      (cond ((= diff 0) a)
            ((= diff 1) (+ a b))
            (else (let ((mid (if (odd? diff)
                           (/ (+ (+ b 1) a) 2)
                           (/ (+ b a) 2))))
              (+ (sum-half a mid)
                 (sum-half (+ mid 1) b))))))))