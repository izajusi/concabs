(load "tree.scm")
(load "trie.scm")

; Exercise 8.30
(define successor-of-in-or
  (lambda (value bst if-none)
    (cond ((empty-tree? bst)
           if-none)
          ((<= (root bst) value)
           (successor-of-in-or value
                               (right-subtree bst)
                               if-none))
          (else
           (successor-of-in-or value
                               (left-subtree bst)
                               (root bst))))))

; Exercise 8.31
(define count-elements-in-bounds
  (lambda (bst low high)
    (cond
      ((empty-tree? bst)
       0)
      ((< high (root bst))
       (count-elements-in-bounds (left-subtree bst) low high))
      ((> low (root bst))
       (count-elements-in-bounds (right-subtree bst) low high))
      (else
       (+ 1
          (count-elements-in-bounds (left-subtree bst) low high)
          (count-elements-in-bounds (right-subtree bst) low high))))))

; Exercise 8.32
(define elements-in-bounds
  (lambda (bst low high)

    (define loop
      (lambda (rem)
        (cond
          ((empty-tree? rem)
           '())
          ((< high (root rem))
           (loop (left-subtree rem)))
          ((> low (root rem))
           (loop (right-subtree rem)))
          (else      
           (merge
            (loop (left-subtree rem))
            (cons
             (root rem)
             (loop (right-subtree rem))))))))

    (loop bst)))

(define bst (sorted-list->min-height-bstree '(1 2 3 3 3 4 4 5)))
bst
(elements-in-bounds bst 2 4)
