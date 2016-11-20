#lang racket
(require 2htdp/universe
         2htdp/image)

(define square-img (rectangle 10 10 'solid 'red))

(struct pos (x y) #:transparent)

(define positions
  (list (pos 14 12) (pos 13 12) (pos 12 12) (pos 11 12) (pos 10 12) (pos 10 11) (pos 10 10) (pos 9 10)))

(define (draw-tile img p scene)
  (match p
    [(pos x y) (place-image img (* x 10) (* y 10) scene)]))

(define (draw-squares l scene)
  (match l
    [(list) scene]
    [(list x xs ...) (draw-squares xs (draw-tile square-img x scene))]))

(define (draw l)
  (draw-squares l (empty-scene 640 480)))

(define (start)
  (big-bang positions
            (to-draw draw)))

(start)

