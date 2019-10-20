#lang racket

(require 2htdp/image)

(define width 400)
(define height 400)
(define center-offset 200) ; so the center of the rose is in the center of the picture
(define scale 200) ; size of the rose

(define (maurer n d)
  ;; creating the rose
  (let ([rose (do ((x1 center-offset)
                   (y1 center-offset)
                   (image (rectangle width height "solid" "white"))
                   (i 0 (+ i 1)))
                ((= i 361) image)
                (let* ([k (* i d)]
                       [r (* scale (sin (degrees->radians (* n k))))]
                       [x2 (+ center-offset (* r (cos (degrees->radians k))))]
                       [y2 (+ center-offset (- (* r (sin (degrees->radians k)))))])
                  (set! image (add-line image x1 y1 x2 y2 "black"))
                  (set! x1 x2)
                  (set! y1 y2)))])

    ;; creating the red function curve over the rose
    (do ((x1 center-offset)
         (y1 center-offset)
         (image rose)
         (i 0 (+ i 1)))
      ((= i 361) image)
      (let* ([k i]
             [r (* scale (sin (degrees->radians (* n k))))]
             [x2 (+ center-offset (* r (cos (degrees->radians k))))]
             [y2 (+ center-offset (- (* r (sin (degrees->radians k)))))])
        (set! image (add-line image x1 y1 x2 y2 (make-pen "red" 3 "solid" "butt" "bevel"))) 
        (set! x1 x2)
        (set! y1 y2)))))

(maurer 6 71)
;(maurer 3 47)
;(maurer 2 39)
;(maurer 5 97)
;(maurer 7 19)
;(maurer 4 31)