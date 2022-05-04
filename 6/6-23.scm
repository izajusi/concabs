(define add-vectors
  (lambda (vec-a vec-b)
    (make-3D-vector
     (+ (x-coord vec-a) (x-coord vec-b))
     (+ (y-coord vec-a) (y-coord vec-b))
     (+ (z-coord vec-a) (z-coord vec-b)))))

(define dot-product
  (lambda (vec-a vec-b)
    (+ (* (x-coord vec-a) (x-coord vec-b))
       (* (y-coord vec-a) (y-coord vec-b))
       (* (z-coord vec-a) (z-coord vec-b)))))

(define scale
  (lambda (vec factor)
    (make-3D-vector (* (x-coord vec-a) factor)
                    (* (y-coord vec-a) factor)
                    (* (z-coord vec-a) factor))))

(define make-3D-vector
  (lambda (x y z)
    (cons x (cons y z))))

(define x-coord
  (lambda (vec)
    (car vec)))

(define y-coord
  (lambda (vec)
    (car (cdr vec))))

(define z-coord
  (lambda (vec)
    (cdr (cdr vec))))

(define vec-a (make-3D-vector 3 4 5))
(define vec-b (make-3D-vector 5 12 13))

(add-vectors vec-a vec-b)
(dot-product vec-a vec-b)
(scale vec-a 2)