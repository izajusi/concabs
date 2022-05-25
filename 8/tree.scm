(define make-empty-tree
  (lambda () '()))

(define make-nonempty-tree
  (lambda (root left-subtree right-subtree)
    (list root left-subtree right-subtree)))

; Make tree with nonempty root but empty left & right subtree
(define make-branchless-tree
  (lambda (root)
    (list root (make-empty-tree) (make-empty-tree))))

(define empty-tree? null?)

(define root car)

(define left-subtree cadr)

(define right-subtree caddr)

(define in?
  (lambda (value tree)
    (cond
      ((empty-tree? tree) #f)
      ((= value (root tree)) #t)
      ((< value (root tree)) (in? value (left-subtree tree)))
      (else ; the value must be greater than the root
       (in? value (right-subtree tree))))))

; Example tree for testing.
(define test-tree
  '(4 
    (2 
     (1 () ()) 
     (3 () ())) 
    (6 
     (5 () ()) 
     (7 () ()))))

; Exercise 8.1
(define minimum
  (lambda (tree)
    (cond
      ((empty-tree? tree) 0)
      ((empty-tree? (left-subtree tree)) (root tree))
      (else (minimum (left-subtree tree))))))

; Exercise 8.2
(define number-of-nodes
  (lambda (tree)
    (if (empty-tree? tree)
        0
        (+ 1
           (number-of-nodes (left-subtree tree))
           (number-of-nodes (right-subtree tree))))))

(define preorder
  (lambda (tree)

    (define preorder-onto
      (lambda (rem acc)
        (if (null? rem)
            acc
            (cons (root rem)
                  (preorder-onto (left-subtree rem)
                                 (preorder-onto (right-subtree rem)
                                                acc))))))

    (preorder-onto tree '())))

; Exercise 8.4
(define inorder
  (lambda (tree)

    (define inorder-onto
      (lambda (rem acc)
        (if (null? rem)
            acc
            (inorder-onto (left-subtree rem)
                          (cons (root rem)
                                (inorder-onto (right-subtree rem)
                                              acc))))))

    (inorder-onto tree '())))

; Exercise 8.5
(define postorder
  (lambda (tree)

    (define postorder-onto
      (lambda (rem acc)
        (if (null? rem)
            acc
            (postorder-onto (left-subtree rem)
                            (postorder-onto (right-subtree rem)
                                            (cons (root rem)
                                                  acc))))))

    (postorder-onto tree '())))

; Exercise 8.6
(define insert
  (lambda (tree value)
    (cond
      ((empty-tree? tree)
       (make-branchless-tree value))
      ((< value (root tree))
       (make-nonempty-tree
        (root tree)
        (insert (left-subtree tree) value)
        (right-subtree tree)))
      (else
       (make-nonempty-tree
        (root tree)
        (left-subtree tree)
        (insert (right-subtree tree) value))))))

; Exercise 8.7
(define list->bstree
  (lambda (lst)

    (define iter
      (lambda (rem acc)
        (if (null? rem)
            acc
            (iter (cdr rem) (insert acc (car rem))))))

    (iter lst (make-empty-tree))))

; Exercise 8.8
(define is-leaf?
  (lambda (tree)
    (and (empty-tree? (left-subtree tree))
         (empty-tree? (right-subtree tree)))))

; Exercise 8.9
(define height
  (lambda (tree)

    (define loop
      (lambda (depth subtree)
        (cond ((empty-tree? subtree) 0)
              ((is-leaf? subtree) depth)
              (else
               (max (loop (+ depth 1) (left-subtree subtree))
                    (loop (+ depth 1) (right-subtree subtree)))))))

    (loop 0 tree)))

(define slice
  (lambda (lst from to)

    (define iter
      (lambda (idx rem acc)
        (cond
          ((null? rem)
           acc)
          ((> idx to)
           acc)
          ((>= idx from)
           (iter (+ idx 1) (cdr rem) (cons (car rem) acc)))
          (else
           (iter (+ idx 1) (cdr rem) acc)))))

    (reverse (iter 0 lst '()))))

; Exercise 8.12
(define sorted-list->min-height-bstree
  (lambda (lst)

    (define split
      (lambda (lst len) ; Assuming that (length lst) >= 3
        (let ((midpoint (quotient len 2)))
          (cons
           (car (slice lst midpoint midpoint))
           (cons
            (slice lst 0 (- midpoint 1))
            (list (slice lst (+ midpoint 1) len)))))))

    (if (null? lst)
        (make-empty-tree)
        (let ((len (length lst)))
          (cond
            ((= len 1)
             (make-branchless-tree (car lst)))
            ((= len 2)
             (make-nonempty-tree
              (cadr lst)
              (make-branchless-tree (car lst))
              (make-empty-tree)))
            (else
             (let ((parts (split lst len)))
               (make-nonempty-tree
                (car parts)
                (sorted-list->min-height-bstree (cadr parts))
                (sorted-list->min-height-bstree (caddr parts))))))))))

; Exercise 8.13
(define optimize-bstree
  (lambda (tree)
    (sorted-list->min-height-bstree (inorder tree))))

; Exercise 8.14
(define sort
  (lambda (lst)
    (inorder (list->bstree lst))))
