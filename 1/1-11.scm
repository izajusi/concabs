(load "pinwheel.scm")

(define nova-my
  (lambda () (overlay
              (filled-triangle -1/2 0 0 1 0 0)
              (filled-triangle 0 0 0 1/2 1 0))))

(define rcross-my
  (lambda ()
    (overlay
    (overlay
     (overlay
      (overlay (filled-triangle -1/2 -1/2 -1/2 1/2 1/2 -1/2)
               (filled-triangle -1/2 1/2 1 1 1 1/2))
      (filled-triangle -1 1 -1/2 1/2 1 1))
     (filled-triangle 1/2 1/2 1 1/2 1/2 -1/2))
    (filled-triangle 1 1/2 1/2 -1/2 1 -1))
    ))

(nova-my)
(rcross-my)

