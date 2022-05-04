(load "my-movie.scm")

(define reverse
  (lambda (lst)
    (define reverse-onto
      (lambda (lst1 lst2)
        (if (null? lst1)
            lst2
            (reverse-onto (cdr lst1)
                          (cons (car lst1)
                                lst2)))))
    (reverse-onto lst '())))

(define length
  (lambda (lst)

    (define iter
      (lambda (n rem)
        (if (null? rem)
            n
            (iter (+ n 1) (cdr rem)))))

    (iter 0 lst)))

(define absorb-n
  (lambda (sentence n)

    (define iter
      (lambda (acc sentence n)
        (cond ((null? sentence) acc)
              ((<= n 0) acc)
              (else
               (iter (cons (car sentence) acc) (cdr sentence) (- n 1))))))

    (reverse (iter '() sentence n))))

(define after-n
  (lambda (sentence n)

    (cond ((null? sentence) '())
          ((<= n 0) sentence)
          (else
           (after-n (cdr sentence) (- n 1))))))

(define query-loop
  (lambda ()
    (newline)
    (newline)
    (let ((query (read)))
      (cond ((exit? query) (display '(see you later)))
            ;; movie-p/a-list is the list of the
            ;;  pattern/action pairs
            (else (answer-by-pattern query movie-p/a-list)
                  (query-loop))))))

(define answer-by-pattern
  (lambda (query p/a-list)
    (cond ((null? p/a-list)
           (display '(i do not understand)))
          ((matches? (pattern (car p/a-list)) query)
           (let ((subs (substitutions-in-to-match
                        (pattern (car p/a-list))
                        query)))
             (let ((result (apply (action (car p/a-list))
                                  subs)))
               (if (null? result)
                   (display '(i do not know))
                   (display result)))))
          (else
           (answer-by-pattern query
                              (cdr p/a-list))))))

; Compares title while discarding symbols such as
; the/a/an.
(define title-match?
  (lambda (a b)

    (define strip-symbol
      (lambda (acc title)
        (cond ((null? title) acc)
              ((member (car title) '(the a an))
               (strip-symbol acc (cdr title)))
              (else
               (strip-symbol (cons (car title) acc) (cdr title))))))

    (equal? (strip-symbol '() a)
            (strip-symbol '() b))))

; Exercise 7.28
(define the-only-element-in
  (lambda (lst)
    (if (null? lst)
        lst
        (let ((next (cdr lst)))
          (if (and (list? next) (null? next))
              (car lst)
              lst)))))

(define movie-p/a-list
  (list
   ; Director of title query.
   (make-pattern/action
    '(who is the director of ...)
    (lambda (title)
      (the-only-element-in
       (movies-satisfying
        our-movie-database
        (lambda (movie) (title-match? (movie-title movie) title))
        movie-director))))

   ; Actor(s) of title query.
   (make-pattern/action
    '(who acted in ...)
    (lambda (title)
      (movies-satisfying
       our-movie-database
       (lambda (movie) (title-match? (movie-title movie) title))
       movie-actors)))

   ; Movies in year query.
   (make-pattern/action
    '(what (movie movies) (was were) made in _)
    (lambda (noun verb year)
      (movies-satisfying 
       our-movie-database
       (lambda (movie) (= (movie-year-made movie) year))
       movie-title)))

   ; Movies in/before/after/since query.
   (make-pattern/action
    '(what (movie movies) (was were) made (in before after since) _)
    (lambda (noun verb symbol year)
      (movies-satisfying
       our-movie-database
       (lambda (movie)
         (cond ((equal? symbol '(in))
                (= (movie-year-made movie) year))
               ((equal? symbol '(before))
                (< (movie-year-made movie) year))
               (else
                (> (movie-year-made movie) year))))
       movie-title)))

   ; Movies made between.
   (make-pattern/action
    '(what (movie movies) (was were) made between _ and _)
    (lambda (noun verb from to)
      (movies-satisfying
       our-movie-database
       (lambda (movie)
         (and (>= (movie-year-made movie) from)
              (<= (movie-year-made movie) to)))
       movie-title)))

   ; Movies made in.
   (make-pattern/action
    '(when was ... made)
    (lambda (title)
      (movies-satisfying
       our-movie-database
       (lambda (movie) (title-match? (movie-title movie) title))
       movie-year-made)))))

; Check if question matches the pattern.
(define matches?
  (lambda (pattern question)
    (cond ((null? pattern)  (null? question))
          ((null? question) #f)
          ((list? (car pattern))
           (if (member (car question) (car pattern))
               (matches? (cdr pattern)
                         (cdr question))
               #f))
          ((equal? (car pattern) '...)
           (let ((n (- (length pattern) 1))
                 (m (length question)))
             (matches? (cdr pattern)
                       (after-n question (- m n)))))             
          ((equal? (car pattern) '_)
           (matches? (cdr pattern)
                     (cdr question)))
          ((equal? (car pattern) (car question))
           (matches? (cdr pattern)
                     (cdr question)))
          (else #f))))

; Exercise 7.26
; Produces substitutions from a question/query based
; on pattern.
(define substitutions-in-to-match
  (lambda (pattern query)

    (define collect-remaining
      (lambda (acc rem-query)
        (if (null? rem-query)
            acc
            (collect-remaining (cons (car rem-query) acc) (cdr rem-query)))))

    (define collect-substitutions
      (lambda (acc rem-pattern rem-query)
    
        (cond ((null? rem-pattern) acc)
              ((null? query) acc)
              ((equal? (car rem-pattern) '...)
               (let ((n (- (length rem-pattern) 1))
                     (m (length rem-query)))
                 (cons (absorb-n rem-query (- m n)) acc)))
              ((equal? (car rem-pattern) '_)
               (collect-substitutions
                (cons (car rem-query) acc) (cdr rem-pattern) (cdr rem-query)))
              ((list? (car rem-pattern))
               (collect-substitutions
                (cons (list (car rem-query)) acc) (cdr rem-pattern) (cdr rem-query)))
              (else
               (collect-substitutions
                acc (cdr rem-pattern) (cdr rem-query))))))

    (reverse (collect-substitutions '() pattern query))))

