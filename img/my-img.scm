(require (lib "fungraph.ss" "concabs"))

; Turn the image 180 degrees.
(define half-turn
  (lambda (img) (quarter-turn-right (quarter-turn-right img))))

; Turn the image 270 degrees;
(define quarter-turn-left
  (lambda (img) (half-turn (quarter-turn-right img))))

; Place two images side-by-side.
(define side-by-side
  (lambda (img1 img2)
    (quarter-turn-left
     (stack (quarter-turn-right img1) (quarter-turn-right img2)
            )
     )))

; Place the image in a pinwheel pattern.
(define pinwheel
  (lambda (img)
    (stack (side-by-side (quarter-turn-right img) (half-turn img))
           (side-by-side img (quarter-turn-left img))
           )))

; Stack n copies of image on top of each other.
(define stack-copies-of
  (lambda (n img)
    (if (= n 1)
        img
        (stack img (stack-copies-of (- n 1) img)))))

; Stack n copies of image on top of each other using iterative method.
(define stack-copies-of-iter
  (lambda (n ori-img img) ; TODO: should be (n img next-img) for more clarity (ori-img is replaced with next-img).
    (if (= n 1)
        img
        (stack-copies-of-iter (- n 1) ori-img (stack ori-img img)))))

(define stack-copies-of-v2
  (lambda (n img)
    (stack-copies-of-iter n img img)))

; Stack n copies of alternating image on top of each other.
(define stack-alt-copies-of
  (lambda (n img)
    (if (= n 1)
        img
        (stack img (stack-alt-copies-of (- n 1) (invert img))))))

; Stack n copies of alternating image on top of each other using iterative method.
(define stack-alt-copies-of-iter
  (lambda (n ori-img img)
    (if (= n 1)
        img
        (stack-alt-copies-of-iter (- n 1) (invert ori-img) (stack img ori-img)))))

(define stack-alt-copies-of-v2
  (lambda (n img)
    (stack-alt-copies-of-iter n (invert img) img)))

; Combine copies of an image in an (w h) pattern.
(define quilt
  (lambda (img w h)
    (if (= w 1)
        (stack-copies-of h img)
        (side-by-side
         (stack-copies-of h img)
         (quilt img (- w 1) h)))))

; Combine copies of an image in an (w h) pattern using iterative method.
(define quilt-iter
  (lambda (h-img w img)
    (if (= w 1) img
        (quilt-iter h-img (- w 1) (side-by-side img h-img)))))

(define quilt-v2
  (lambda (img w h)
    (quilt-iter (stack-copies-of-v2 h img) w (stack-copies-of-v2 h img))))

; Combine copies of an image in an (w h) checkboard pattern.
(define checkboard
  (lambda (img w h)
    (if (= w 1)
        (stack-alt-copies-of h img)
        (side-by-side
         (stack-alt-copies-of h img)
         (checkboard (invert img) (- w 1) h)))))

; Combine copies of an image in an (w h) checkboardb pattern using iterative method.
(define checkboard-iter
  (lambda (h-img w img)
    (if (= w 1) img
        (checkboard-iter (invert h-img) (- w 1) (side-by-side img h-img)))))

(define checkboard-v2
  (lambda (img w h)
    (checkboard-iter (invert (stack-alt-copies-of-v2 h img)) w (stack-alt-copies-of-v2 h img))))
