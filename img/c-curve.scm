(require (lib "fungraph.ss" "concabs"))

(define c-curve
  (lambda (x0 y0 x1 y1 level)
    (if (= level 0)
        (line x0 y0 x1 y1)
        (let ((xmid (/ (+ x0 x1) 2))
              (ymid (/ (+ y0 y1) 2))
              (dx (- x1 x0))
              (dy (- y1 y0)))
          (let ((xa (- xmid (/ dy 2)))
                (ya (+ ymid (/ dx 2))))
            (overlay (c-curve x0 y0 xa ya (- level 1))
                     (c-curve xa ya x1 y1 (- level 1))))))))

(define min-x-of-c-curve
  (lambda (x0 y0 x1 y1 level)
    (if (= level 0)
        (min x0 x1)
        (let ((xmid (/ (+ x0 x1) 2))
              (ymid (/ (+ y0 y1) 2))
              (dx (- x1 x0))
              (dy (- y1 y0)))
          (let ((xa (- xmid (/ dy 2)))
                (ya (+ ymid (/ dx 2))))
            (min (min-x-of-c-curve x0 y0 xa ya (- level 1))
                 (min-x-of-c-curve xa ya x1 y1 (- level 1))))))))

(c-curve 0 -1/2 0 1/2 5)
(min-x-of-c-curve 0 -1/2 0 1/2 5)

(define f (lambda (level) (c-curve 0 -1/2 0 1/2 level)))

(define dist
  (lambda (x0 y0 x1 y1)
    (sqrt (+ (expt (- x1 x0) 2)
             (expt (- y1 y0) 2)))))

(define half-dist
  (lambda (x0 y0 x1 y1)
    (let ((xmid (/ (+ x0 x1) 2))
          (ymid (/ (+ y0 y1) 2))
          (dx (- x1 x0))
          (dy (- y1 y0)))
      (let ((xa (- xmid (/ dy 2)))
            (ya (+ ymid (/ dx 2))))
        (dist x0 y0 xa ya)))))

(define length-of-c-curve
  (lambda (x0 y0 x1 y1 level)
    (if (= level 0)
        (dist x0 y0 x1 y1)
        (let ((xmid (/ (+ x0 x1) 2))
              (ymid (/ (+ y0 y1) 2))
              (dx (- x1 x0))
              (dy (- y1 y0)))
          (let ((xa (- xmid (/ dy 2)))
                (ya (+ ymid (/ dx 2))))
            (+ (length-of-c-curve x0 y0 xa ya (- level 1))
               (length-of-c-curve xa ya x1 y1 (- level 1))))))))

(define f-len (lambda (level) (length-of-c-curve 0 -1 0 1 level)))

(define my-fract
  (lambda (x0 y0 x1 y1 level)
    (if (= level 0)
        (line x0 y0 x1 y1)
        (let ((x-1 (+ (* x0 3/4) (* x1 1/4)))
              (x-3 (+ (* x0 1/4) (* x1 3/4)))
              (y-1 (+ (* y0 3/4) (* y1 1/4)))
              (y-3 (+ (* y0 1/4) (* y1 3/4)))
              (dx (- x1 x0))
              (dy (- y1 y0)))
          (let ((xa (- x-1 (/ dy 2)))
                (ya (+ y-1 (/ dx 2)))
                (xb (- x-3 (/ dy 2)))
                (yb (+ y-3 (/ dx 2))))
            (overlay (my-fract x0 y0 xa ya (- level 1))
                     (overlay
                      (my-fract xa ya xb yb (- level 1))
                      (my-fract xb yb x1 y1 (- level 1)))))))))
        

(define my-f (lambda (level) (my-fract 0 -1/2 0 1/2 level)))