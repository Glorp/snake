#lang racket
(provide random-stream)

(define (random-stream seed min max)
  (define rng (make-rng seed))
  (let loop ()
    (define current (random min max rng))
    (stream-cons current (loop))))

(define (make-rng seed)
  (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
    (random-seed seed)
    (current-pseudo-random-generator)))



(module+ test

  (define (stream-take->list str count)
    (match count
      [0 '()]
      [c (cons (stream-first str)
               (stream-take->list (stream-rest str) (- c 1)))]))
  
  (require rackunit)
  
  (check-equal? (stream-take->list (random-stream 100 10 20) 10)
                (stream-take->list (random-stream 100 10 20) 10))

  (check-equal? (stream-take->list (random-stream 100 10 25) 10)
                (stream-take->list (random-stream 100 10 25) 10))

  (check-not-equal? (stream-take->list (random-stream 100 10 20) 10)
                    (stream-take->list (random-stream 100 10 25) 10)))
