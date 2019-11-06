#lang racket/base

(require 2htdp/image 2htdp/universe graphics/graphics)

(define head (rectangle 20 20 "solid" "red"))
(define body-piece (rectangle 20 20 "solid" "white"))
(define fruit (circle 10 "solid" "green"))
(define width 600)
(define height 600)
(define canvas (rectangle width height "solid" "black"))


;;; a world is the game: snake - list of posn , movement - string, food - posn, score - number
;;; movement is up, down, left, right
(define-struct world (snake movement food score))

(define (random-fruit) (make-posn (+ 10 (* 20 (random 30))) (+ 10 (* 20 (random 30))))) ; generate a random fruit

(define first-world (make-world (list (make-posn (+ 10 (/ width 2)) (+ 10 (/ height 2))))
                                "right"
                                (random-fruit)
                                0))

;;; draw procedure world -> image
(define (draw-game w)
  (let* ([fruit-image (place-image fruit (posn-x (world-food w)) (posn-y (world-food w)) canvas)]
         [head-image (place-image head (posn-x (car (world-snake w))) (posn-y (car (world-snake w))) fruit-image)]
         [snake-body-image (draw-snake-body (cdr (world-snake w)) head-image)]
         [score-image (place-image (text (number->string (world-score w)) 20 "white") 100 30 snake-body-image)]
         [final-image (place-image (text "SCORE:" 20 "white") 50 30 score-image)])
    final-image))

;;; update procedure world -> world
(define (update-game w)
  (if (and (and (and (< (posn-x (car (world-snake w))) width) (> (posn-x (car (world-snake w))) 0))
                (and (< (posn-y (car (world-snake w))) height) (> (posn-y (car (world-snake w))) 0))) ; check boundaries
           (not (check-endgame (cdr (world-snake w)) (car (world-snake w))))) ; check snake intersection with it's tail
      (if (and (equal? (posn-x (world-food w)) (posn-x (car (world-snake w))))
               (equal? (posn-y (world-food w)) (posn-y (car (world-snake w))))) ; check intersection with the fruit
          (make-world (new-snake-list (world-snake w) "eaten" (world-movement w) 1 (car (world-snake w)))
                      (world-movement w)
                      (random-fruit)
                      (+ (world-score w) 1))
          (make-world (new-snake-list (world-snake w) "not-eaten" (world-movement w) 1 (car (world-snake w)))
                      (world-movement w)
                      (world-food w)
                      (world-score w)))
      first-world))

;;; handle a keyboard event
;;; world key-event -> world
(define (key-check w ke)
  (if (or (string=? ke "up") (string=? ke "down") (string=? ke "left") (string=? ke "right"))
      (make-world (world-snake w) ke (world-food w) (world-score w))
      w))

(define (draw-snake-body lst image)
  (if (null? lst)
      image
      (draw-snake-body (cdr lst) (place-image body-piece (posn-x (car lst)) (posn-y (car lst)) image))))

(define (check-endgame lst head)
  (if (null? lst)
      #f
      (if (and (equal? (posn-x (car lst)) (posn-x head)) (equal? (posn-y (car lst)) (posn-y head)))
          #t
          (check-endgame (cdr lst) head))))

;;; updates the head position
(define (move-head pos dir)
  (cond [(string=? dir "up") (make-posn (posn-x pos) (- (posn-y pos) 20))]
        [(string=? dir "down") (make-posn (posn-x pos) (+ (posn-y pos) 20))]
        [(string=? dir "left") (make-posn (- (posn-x pos) 20) (posn-y pos))]
        [(string=? dir "right") (make-posn (+ (posn-x pos) 20) (posn-y pos))]))

;;; creates a new list in which each block takes the coordinates of that in front of it
(define (new-snake-list lst eat dir first previous)
  (if (string=? eat "eaten")
      (new-snake-list (append lst (list (make-posn 0 0))) "not-eaten" dir first previous) ; simple posn because it is updated again before next draw
      (if (null? lst)
          '()
          (if (equal? first 1)
              (cons (move-head (car lst) dir) (new-snake-list (cdr lst) eat dir 0 previous))
              (cons previous (new-snake-list (cdr lst) eat dir 0 (car lst)))))))

;;; main program
(big-bang first-world ; initial world
  (to-draw draw-game)
  (on-tick update-game 1/6) ; update-game and frame-rate
  (on-key key-check))