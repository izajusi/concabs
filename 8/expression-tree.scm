(define make-constant
  (lambda (x) x))

(define constant? number?)

(define make-expr
  (lambda (left-operand operator right-operand)
    (list left-operand operator right-operand)))

(define operator cadr)

(define left-operand car)

(define right-operand caddr)

(define empty? null?)

(define is-leaf? number?)

(define evaluate
  (lambda (expr)
    (cond ((constant? expr) expr)
          (else ((look-up-value (operator expr))
                 (evaluate (left-operand expr))
                 (evaluate (right-operand expr)))))))

(define look-up-value
  (lambda (name)
    (cond ((equal? name '+) +)
          ((equal? name '*) *)
          ((equal? name '-) -)
          ((equal? name '/) /)
          (else (error "Unrecognized name" name)))))

; Exercise 8.15
(define evaluate-example
  (lambda ()
    (evaluate
     (make-expr 1 '+
                (make-expr 2 '*
                           (make-expr 3 '- 5))))))

; Exercise 8.16
(define operators-used-in
  (lambda (tree)

    (define iter
      (lambda (rem acc)
        (if (is-leaf? rem)
            acc
            (cons (operator rem)
                  (iter (left-operand rem)
                        (iter (right-operand rem) acc))))))

    (iter tree '())))

; Exercise 8.17
(define number-of-operations
  (lambda (tree)
    (if (is-leaf? tree)
        0
        (+ 1
           (number-of-operations (left-operand tree))
           (number-of-operations (right-operand tree))))))
