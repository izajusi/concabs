(define servings
  (lambda (weight)
    (if (>= weight 12)
        (/ weight 1/2)
        (round (/ weight 3/4)))))

(servings 12)
(servings 9)