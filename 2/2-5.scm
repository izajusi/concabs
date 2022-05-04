(define multiply
  (lambda (n x)
         (if (= x 0)
             0
             (+ n (multiply n (- x 1))))))
         
