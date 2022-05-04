(define make-schedule-item
  (lambda (room course time)
    (cons room (cons course time))))

(define room
  (lambda (item)
    (car item)))

(define course
  (lambda (item)
    (car (cdr item))))

(define time
  (lambda (item)
    (cdr (cdr item))))

(define item (make-schedule-item 'OH321 'MC27 1230))
(room item)
(course item)
(time item)