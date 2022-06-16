(define make-empty-trie
  (lambda () '()))

(define make-nonempty-trie
  (lambda (root-values ordered-subtries)
    (list root-values ordered-subtries)))

(define empty-trie? null?)

(define root-values car)

(define subtries cadr)

(define subtrie-with-label
  (lambda (trie label)
    (list-ref (subtries trie) (- label 2))))

(define make-person
  (lambda (name phone-number)
    (list name phone-number)))

(define name car)

(define phone-number cadr)

(define look-up-with-menu
  (lambda (phone-trie)
    (menu)
    (look-up-phone-number phone-trie)))

(define menu
  (lambda ()
    (newline)
    (display "Enter the name, one digit at a time.")
    (newline)
    (display "Indicate you are done by 0.")
    (newline)))

(define display-phone-numbers
  (lambda (people)
    (define display-loop
      (lambda (people)
        (cond ((null? people) 'done)
              (else (newline)
                    (display (name (car people)))
                    (display "'s phone number is ")
                    (display (phone-number (car people)))
                    (display-loop (cdr people))))))
    (if (null? people)
        (display "Sorry we can't find that name.")
        (display-loop people))))

(define phone-trie '(() ((() ((() (() (() ((() (() () () () () () (() (() () () () () (() (() () () () () () () (((cadbury 7464)) (() () () () () () () ())))) () ())) ())) () () () () () () ())) () (() (() (() (() () () () () (((baker 7465)) (() () () () () () () ())) () ())) () (() (() (() ((() ((() (() () () () () () (() (() () () () () () (((callebaut 7480)) (() () () () () () () ())) ())) ())) () () () () () () ())) () () () () () () ())) () () () () () ())) () () () ())) () () () ())) () () () () () () ())) () (() (() (() (() () () () () (() (() () () () () (() (() () (() (() (() (() () () () () () () (((hershey 7482)) (() () () () () () () ())))) () () () () () ())) () () () () ())) () ())) () ())) (() (() () (() (() () () () () (() ((() (() (() (() (() (() () () (() (() () () (() (() () (((ghiradelli 7476)) (() () () () () () () ())) () () () () ())) () () () ())) () () () ())) () () () () () ())) () () () () () ())) () () () () () () ())) () ())) () () () () ())) () () () () ())) (() (() () (() (() () () () (() (() (() (() () () () () () (((lindt 7483)) (() () () () () () () ())) ())) () () () () () ())) () () ())) () () () () ())) (() ((() (() () (() (() () () (() (() () () (() ((() (() () () () () (() (() (((maillard 7477)) (() () () () () () () ())) () () () () () ())) () ())) () () () () () () ())) () () () ())) () () () ())) () () () () ())) (() (() () () () () (() (() () () (() (() (() (() () () () (() (() () () () () (((merkens 7469)) (() () () () () () () ())) () ())) () () ())) () () () () () ())) () () () ())) () ())) () () () () () ())) (() (() (() (() (((see 7463)) (() () () () () () () ())) () () () (() (() () () () () () (() (() () (() (() () (() (() () () () (() ((((perugina 7007)) (() () () () () () () ())) () () () () () () ())) () () ())) () () () () ())) () () () () ())) ())) () ())) (() (() () () () () () (() (() () () () () () (() (() (() (() () () () () (((ritter 7479)) (() () () () () () () ())) () ())) () () () () () ())) ())) ())) () () (() (() () () () () (() (() () () () () () (() (() (() (() () () () (() (() () (() (() () () (() (() () (((spruengli 7009)) (() () () () () () () ())) () () () () ())) () () () ())) () () () () ())) () () ())) () () () () () ())) ())) () ())) (() ((() (() () (() ((() (() () () () () (() (() (((suchard 7654)) (() () () () () () () ())) () () () () () ())) () ())) () () () () () () ())) () () () () ())) () () () () () () ())) ())) (() (() () () () (() ((() (() () () (() (() (() (() () () () () (((tobler 7481)) (() () () () () () () ())) () ())) () () () () () ())) () () () ())) () () () () () () ())) () () ())) (() (() () (() (() () () (() ((() (() () () () () () (() (() () () () () (((wilbur 7466)) (() () () () () () () ())) () ())) ())) () () () () () () ())) () () () ())) () () () () ())))))

; Exercise 8.18
(define number-in-trie
  (lambda (trie)

    (define length
      (lambda (rem acc)
        (if (null? rem)
            acc
            (length (cdr rem) (+ acc 1)))))

    (define sum
      (lambda (rem acc)
        (if (null? rem)
            acc
            (sum (cdr rem) (+ acc (car rem))))))

    (if (null? trie)
        0
        (+ (length (root-values trie) 0)
           (sum (map
                 (lambda (subtrie) (number-in-trie subtrie))
                 (subtries trie))
                0)))))

; Merges two list; a and b; starting with elements of list a
; followed by elements by list b in order.
;
; Example
; (merge '(1 2 3) '(4 5 6)) => (1 2 3 4 5 6)
(define merge
  (lambda (a b)

    (define iter
      (lambda (rem acc)
        (if (null? rem)
            acc
            (iter
             (cdr rem)
             (cons (car rem) acc)))))

    (iter (reverse a) b)))

; Flatten a list of list into a single list containing all elements
; of the sublist in order.
;
; Example:
; (flatten '((1 2) (3 4) (5 6))) => (1 2 3 4 5 6)
(define flatten
  (lambda (lst)

    (define iter
      (lambda (rem acc)
        (if (null? rem)
            acc
            (iter
             (cdr rem)
             (merge acc (car rem))))))

    (iter lst '())))


; Exercise 8.19
(define values-in-trie
  (lambda (trie)
    (if (null? trie)
        '()
        (merge
         (root-values trie)
         (flatten (map values-in-trie (subtries trie)))))))

; Exercise 8.20
(define look-up-phone-number
  (lambda (phone-trie)
    (newline)

    (let ((num (number-in-trie phone-trie)))
      (cond ((= num 0)
             (display "Sorry we can't find that name."))
            ((= num 1)
             (display-phone-numbers (values-in-trie phone-trie)))
            (else
             (let ((user-input (read)))
               (cond ((= user-input 0)
                      (display-phone-numbers (root-values phone-trie)))
                     ((= user-input 1)
                      (display-phone-numbers (values-in-trie phone-trie))
                      (look-up-phone-number phone-trie))
                     (else (look-up-phone-number (subtrie-with-label
                                                  phone-trie
                                                  user-input))))))))))

; Exercise 8.21
(define letter->number
  (lambda (letter)
    (cond
      ((member letter '(a b c)) 2)
      ((member letter '(d e f)) 3)
      ((member letter '(g h i)) 4)
      ((member letter '(j k l)) 5)
      ((member letter '(m n o)) 6)
      ((member letter '(p q r s)) 7)
      ((member letter '(t u v)) 8)
      ((member letter '(w x y z)) 9))))

; Exercise 8.22
(define name->labels
  (lambda (name)
    (define explode-symbol
      (lambda (sym)
        (map string->symbol
             (map string
                  (string->list (symbol->string sym))))))

    (map letter->number (explode-symbol name))))

(define make-labeled-value
  (lambda (labels value)
    (list labels value)))

(define labels car)

(define value cadr)

; Exercise 8.23
(define empty-labels?
  (lambda (labeled-value)
    (null? (labels labeled-value))))

; Exercise 8.24
(define first-label
  (lambda (labeled-value)
    (car (labels labeled-value))))

; Exercise 8.25
(define strip-one-label
  (lambda (labeled-value)
    (if (empty-labels? labeled-value)
        labeled-value
        (make-labeled-value
         (cdr (labels labeled-value))
         (value labeled-value)))))

; Exercise 8.26
(define value->labeled-value
  (lambda (value)
    (make-labeled-value
     (name->labels (name value))
     value)))

; Exercise 8.27
(define values-with-first-label
  (lambda (labeled-values label)
    (map strip-one-label
         (filter
          (lambda (labeled-value)
            (= label (first-label labeled-value)))
          labeled-values))))

; Exercise 8.28
(define categorize-by-first-label
  (lambda (labeled-values)
    (map
     (lambda (label)
       (values-with-first-label labeled-values label))
     '(2 3 4 5 6 7 8 9))))

; Exercise 8.29
(define remove
  (lambda (predicate lst)
    (filter
     (lambda (elem)
       (not (predicate elem)))
     lst)))

(define labeled-values->trie
  (lambda (labeled-values)
    (if (null? labeled-values)
        (make-empty-trie)
        (make-nonempty-trie
         (map value
              (filter empty-labels? labeled-values))
         (map labeled-values->trie
              (categorize-by-first-label
               (remove empty-labels? labeled-values)))))))

(define make-phone-trie
  (lambda ()
    (labeled-values->trie
     (map value->labeled-value
          (list
           (make-person 'lindt             7483)
           (make-person 'cadbury           7464)
           (make-person 'wilbur            7466)
           (make-person 'hershey           7482)
           (make-person 'spruengli         7009)
           (make-person 'merkens           7469)
           (make-person 'baker             7465)
           (make-person 'ghiradelli        7476)
           (make-person 'tobler            7481)
           (make-person 'suchard           7654)
           (make-person 'callebaut         7480)
           (make-person 'ritter            7479)
           (make-person 'maillard          7477)
           (make-person 'see               7463)
           (make-person 'perugina          7007))))))
