; Exercise 5.15
(define make-function-with-exception
 (lambda (except return f)
   (lambda (n)
     (if (= n except) return (f n)))))

(define usually-sqrt
  (make-function-with-exception 7 2 sqrt))

(usually-sqrt 9)
(usually-sqrt 16)
(usually-sqrt 7)