(define make-interval
  (lambda (low high)
    (cons low high)))

(define upper-endpoint
  (lambda (interval)
    (cdr interval)))

(define lower-endpoint
  (lambda (interval)
    (car interval)))

(define mid-point
  (lambda (interval)
    (/ (+ (upper-endpoint interval)
          (lower-endpoint interval)) 2)))

(define right-half
  (lambda (interval)
    (cons (mid-point interval) (upper-endpoint interval))))

(define my-interval (make-interval 3 7))
(upper-endpoint my-interval)
(lower-endpoint my-interval)
(mid-point my-interval)
(right-half my-interval)