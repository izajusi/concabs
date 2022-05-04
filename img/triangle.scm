(require (lib "fungraph.ss" "concabs"))

(define triangle
  (lambda (x0 y0 x1 y1 x2 y2)
    (overlay
     (line x0 y0 x1 y1)
     (overlay
      (line x1 y1 x2 y2)
      (line x2 y2 x0 y0)))))

(define sg
  (lambda (x0 y0 x1 y1 x2 y2 level)
  (define mid
    (lambda (a b)
      (/ (+ a b) 2)))

    (if (= 0 level)
        (triangle x0 y0 x1 y1 x2 y2)
        (let ((xa (mid x0 x1))
              (ya (mid y0 y1))
              (xb (mid x1 x2))
              (yb (mid y1 y2))
              (xc (mid x2 x0))
              (yc (mid y2 y0)))
          (overlay
           (sg x0 y0 xa ya xc yc (- level 1))
           (overlay
            (sg xa ya x1 y1 xb yb (- level 1))
            (sg xc yc xb yb x2 y2 (- level 1))))))))

(define f
  (lambda (level)
    (sg -1 -.75 1 -.75 0 1 level)))
              