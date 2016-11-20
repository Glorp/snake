#lang racket
(require 2htdp/universe
         2htdp/image
         "random-stream.rkt")

(define square-img (rectangle 10 10 'solid 'red))

(struct world (positions random-stream) #:transparent)
(struct pos (x y) #:transparent)

(define start-world (world (list)
                           (random-stream 100 1 64)))

(define (draw-tile img p scene)
  (match p
    [(pos x y) (place-image img (* x 10) (* y 10) scene)]))

(define (draw-squares l scene)
  (match l
    [(list) scene]
    [(list x xs ...) (draw-squares xs (draw-tile square-img x scene))]))

(define (draw w)
  (match w
    [(world l _) (draw-squares l (empty-scene 640 640))]))


(define (stream->pos/stream str)
  (define x (stream-first str))
  (define y (stream-first (stream-rest str)))
  (define rest-str (stream-rest (stream-rest str)))
  (list (pos x y) rest-str))

(define (update w)
  (match w
    [(world positions str)
     (match (stream->pos/stream str)
       [(list new-position rest-str)
        (define new-positions (cons new-position positions))
        (world new-positions rest-str)])]))


(define (start)
  (big-bang start-world
            (to-draw draw)
            (on-tick update 1/2)))

(start)

