(define make-point
  (lambda (x y)
    (cons x y)))

(define x-coord
  (lambda (point)
    (car point)))

(define y-coord
  (lambda (point)
    (cdr point)))

(define distance
  (lambda (point-a point-b)
    (sqrt (+ (expt (- (x-coord point-b)
                      (x-coord point-a)) 2)
             (expt (- (y-coord point-b)
                      (y-coord point-a)) 2)))))

(define point-a (make-point 0 0))
(define point-b (make-point 3 4))
(distance point-a point-b)