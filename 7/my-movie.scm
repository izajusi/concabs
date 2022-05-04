(load "movie.scm")

; Exercise 7.22.a
(define movies-made-in-year
  (lambda (movie-lst year)
    (filter (lambda (movie)
              (= year (movie-year-made movie)))
            movie-lst)))

; Exercise 7.22.b and 7.22.c
(define movies-directed-by
  (lambda (movie-lst director)
    (filter (lambda (movie)
              (equal? director (movie-director movie)))
            movie-lst)))

; Exercise 7.22.d
(define movies-with-actor
  (lambda (movie-lst actor)
    (filter (lambda (movie)
              (member actor (movie-actors movie)))
            movie-lst)))

; Exercise 7.23
(define titles-of-movies-satisfying
  (lambda (movie-lst ok?)
    (map (lambda (movie) (movie-title movie))
         (filter ok? movie-lst))))

; Exercise 7.24
(define movies-satisfying
  (lambda (movie-lst predicate selector)
    (map (lambda (movie) (selector movie))
         (filter predicate movie-lst))))
