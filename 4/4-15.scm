; Maximum weighings needed to find a fake coin by using
; a balance scale.
(define max-weighings
  (lambda (n)
    (cond ((= n 1) 0)
          ((odd? n) (max-weighings (- n 1)))
          (else (+ 1 (max-weighings (/ n 2)))))))

(define max-weighings-3
  (lambda (n)
    (let ((m (modulo n 3)))
      (cond ((= n 1) 0)
            ((= n 2) 1) 
            ((= m 2) (+ 1 (max-weighings-3 (- n 2))))
            ((= m 1) (+ 1(max-weighings-3 (- n 1))))
            (else (+ 1 (max-weighings-3 (/ n 3))))))))
