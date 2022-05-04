(define candy-temperature
  (lambda (temp elev)
    (+ temp (round (/ elev 500)))))

(define mile-to-feet
  (lambda (mile)
    (* mile 5280)))

(candy-temperature 244 (mile-to-feet 1))