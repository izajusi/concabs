(define survives?
  (lambda (pos n)
    (define renumber
      (lambda (old-pos old-n)
        (let ((next-pos (- old-pos 3)))
          (if (> 0 next-pos) (+ old-n next-pos) next-pos))))

    (if (< n 3)
        #t
        (if (= pos 3)
            #f
            (survives? (renumber pos n) (- n 1))))))

(define find-survivor-after
  (lambda (pos n)
    (let ((next (+ pos 1)))
      (cond ((> pos n) -1)
            ((survives? next n) next)
            (else (find-survivor-after next n))))))
