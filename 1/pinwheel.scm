(require (lib "fungraph.ss" "concabs"))
(load "quilting.scm")

(define half-turn
  (lambda (img) (quarter-turn-right (quarter-turn-right img))))

(define quarter-turn-left
  (lambda (img) (half-turn (quarter-turn-right img))))

(define side-by-side
  (lambda (img1 img2)
    (quarter-turn-left
     (stack (quarter-turn-right img1) (quarter-turn-right img2)
            )
     )))

(define pinwheel
  (lambda (img)
    (stack (side-by-side (quarter-turn-right img) (half-turn img))
           (side-by-side img (quarter-turn-left img))
           )))
