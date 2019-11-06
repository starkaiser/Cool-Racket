#lang racket

(require 2htdp/image 2htdp/universe graphics/graphics)

(define width 800)
(define height 600)
(define canvas (rectangle width height "solid" (color 120 242 252)))
(define ground (rectangle width 25 "solid" (color 2 252 19)))
(define pipe (rectangle 50 height "solid" "black"))
(define gate (rectangle 50 150 "solid" (color 120 242 252)))
(define cool-racket (circle 25 "solid" "red")) ; the png image can also be used
(define gravity 1.5)
(define lift 20)

;;; world: player-posn, velocity-number, pipes-x-coord of pipes, gates-random y-coord of gates
(define-struct world (player velocity pipes gates))

;;; there will be 5 pipes and when the first one gets past the left side it will be deleted and other created
(define first-world (make-world (make-posn 50 (/ height 2)) 0 (list 350 600 800 1000 1200) (list (* 100 (random 6))
                                                                                                 (* 100 (random 6))
                                                                                                 (* 100 (random 6))
                                                                                                 (* 100 (random 6))
                                                                                                 (* 100 (random 6)))))

(define (draw-pipes lst image)
  (if (null? lst)
      image
      (draw-pipes (cdr lst) (place-image pipe (car lst) (/ height 2) image))))

(define (draw-gates lst1 lst2 image)
  (if (null? lst1)
      image
      (draw-gates (cdr lst1) (cdr lst2) (place-image gate (car lst2) (car lst1) image))))

;;; world -> image
(define (draw-game w)
  (let* ([ground-image (place-image ground (/ width 2) (- height 12.5) canvas)]
         [pipes-image (draw-pipes (world-pipes w) ground-image)]
         [gates-image (draw-gates (world-gates w) (world-pipes w) pipes-image)]
         [final-image (place-image cool-racket (posn-x (world-player w)) (posn-y (world-player w)) gates-image)])
    final-image))

;;; world -> world
(define (update-game w)
  (if (and (> (posn-x (world-player w)) (- (car (world-pipes w)) 50))
           (< (posn-x (world-player w)) (+ (car (world-pipes w)) 45))
           (or (< (posn-y (world-player w)) (- (car (world-gates w)) 50))
               (> (posn-y (world-player w)) (+ (car (world-gates w)) 50))))
      ;; restart the game if the player hits the pipes
      (make-world (make-posn 50 (/ height 2)) 0 (list 350 600 800 1000 1200) (list (* 100 (random 6))
                                                                                   (* 100 (random 6))
                                                                                   (* 100 (random 6))
                                                                                   (* 100 (random 6))
                                                                                   (* 100 (random 6))))
      ;; calculete the positions of the player and pipes
      (let ([new-y (+ (posn-y (world-player w)) (+ (world-velocity w) gravity))]
            [new-velocity (* 0.9 (+ (world-velocity w) gravity))]
            [new-pipes (map (λ (pipe) (- pipe 3)) (world-pipes w))])
        (make-world (make-posn 50 (if (or (> new-y height) (< new-y 0))
                                      (posn-y (world-player w))
                                      new-y))
                    new-velocity
                    (if (< (car new-pipes) -25)
                        (reverse (cons (+ 250 (car (reverse new-pipes))) (reverse (cdr new-pipes))))
                        new-pipes) ; delete the first pipe that exits the screen and add a new one
                    (if (< (car new-pipes) -25)
                        (reverse (cons (* 100 (random 5)) (reverse (cdr (world-gates w)))))
                        (world-gates w)))))) ; delete the first gate that exits the screen and add a new one

;;; world key-event -> world
(define (key-check w ke)
  (if (string=? ke "up")
      (make-world (make-posn (posn-x (world-player w))
                             (posn-y (world-player w)))
                  (- (world-velocity w) lift)
                  (world-pipes w)
                  (world-gates w))
      w))

(big-bang first-world
  (to-draw draw-game)
  (on-tick update-game)
  (on-key key-check))