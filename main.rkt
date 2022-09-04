#lang racket

(require threading math)

(struct posn (x y) #:transparent)

(struct size (radius) #:transparent)

(struct component (name attributes) #:transparent)

(struct entity (id components) #:transparent)

(struct world (entities components systems db) #:transparent)

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

; Entities - Index mapping entity IDs to components
; Components - Index mapping component names to entity ids

; Returns an empty world
(define (new-world)
  (world (make-hash) (make-hash) null null))

; Returns a uuid string
(define (add-new-entity w)
  ((uuid-string))

; Returns a new world after running all systems in order
(define (run-all-systems world)
  (let ([systems (world-systems world)])
    (foldl (lambda (w sys) (sys w)) world systems)))

(define (new-component)
  (make-hash))

(define (add-component w ent-id comp)
  (let ([ents (world-entities w)]
        [ent-comps (hash-ref ents ent-id)]
        [new-ents (hash-set ent-comps ent-id (cons comp ent-comps))]
        [components (world-components w)]
        [new-components (hash-update components 
        
  

(define start (posn 0 0))
(define movedir (posn 10 8))
(define secondmove (posn -5 -6))
(define testsize (size 0))

(display
 (~> start
     (move movedir)
     (move secondmove)
     (distance (posn 2 2))))
