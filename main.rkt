#lang racket

(require threading math)

(struct posn (x y) #:transparent)

(struct size (radius) #:transparent)

; move : posn posn -> posn
(define (move from vec2)
  (let ([x (+ (posn-x from) (posn-x vec2))]
        [y (+ (posn-y from) (posn-y vec2))])
    (posn x y)))

; distance : posn posn -> Number
(define (distance from to)
  (let ([dx (- (posn-x from) (posn-x to))]
        [dy (- (posn-y from) (posn-y to))])
    (sqrt (+ (expt dx 2) (expt dy 2)))))

; touching? : posn Natural posn Natural -> Boolean
(define (touching? p1 s1 p2 s2)
  (let ([dist (distance p1 p2)])
    (<= dist (+ s1 s2))))

; Index mapping entity IDs to components
(define entities (make-hash))

; Index mapping component names to entity ids
(define components  (make-hash))

(define start (posn 0 0))
(define movedir (posn 10 8))
(define secondmove (posn -5 -6))
(define testsize (size 0))

(display
 (~> start
     (move movedir)
     (move secondmove)
     (distance (posn 2 2))))