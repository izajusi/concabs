(require (lib "fungraph.ss" "concabs"))
(load "my-img.scm")
(load "digits.scm")

(define image-of-digit
  (lambda (d)
    (cond ((= d 0) zero-bb)
          ((= d 1) one-bb)
          ((= d 2) two-bb)
          ((= d 3) three-bb)
          ((= d 4) four-bb)
          ((= d 5) five-bb)
          ((= d 6) six-bb)
          ((= d 7) seven-bb)
          ((= d 8) eight-bb)
          ((= d 9) nine-bb))))

(define image-of-number
  (lambda (d)
    (if (< d 10)
        (image-of-digit d)
        (side-by-side (image-of-number (quotient d 10))
                      (image-of-digit (remainder d 10))))))
  