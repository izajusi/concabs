(define tax
  (lambda (income)
    (if (< income 10000)
        0
        (* 20/100 (- income 10000)))))

(tax 12500)