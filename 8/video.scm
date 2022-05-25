(load "tree.scm")

; Exercise 8.3
(define list-by-key
  (lambda (key-value comparator tree)

    (define collect-onto
      (lambda (rem acc)
        (if (empty-tree? rem)
            acc
            (let ((comparison-result (comparator (root rem)
                                                 key-value)))

              (cond
                ((equal? comparison-result '=)
                 (cons (root rem)
                       (collect-onto (left-subtree rem)
                                     (collect-onto (right-subtree)
                                                   acc))))
                ((equal? comparison-result '<)
                 (collect-onto (left-subtree rem) acc))
                (else
                 (collect-onto (right-subtree rem) acc)))))))

    (collect-onto tree '())))
