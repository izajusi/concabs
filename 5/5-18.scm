(define make-scaled
  (lambda (scale f)
    (lambda (x)
      (* scale (f x)))))

(define add-one
  (lambda (x)
    (+ 1 x)))

(define mystery
  (make-scaled 3 add-one))

(mystery 4) ; returns 15

((make-scaled 2 (make-scaled
                 3 add-one)) 4) ; returns 30