#lang racket

(require threading math lens uuid)

(struct component (type attrs) #:transparent)

(define (new-world)
  (hash
   'step 0 ; a world step counter, think of it like time
   'entities null ; Entities hashmap UUIDs to Components
   'components null ; Components hashmap component "types" to entity UUIDs
   'systems null ; Systems list of functions that work on entities/components
   'db null)) ; db is a caching queue for entity addition/removal etc

(define (get-entity world id)
  (lens-view (hash-ref-nested-lens 'entities id) world))

; Returns a uuid string
(define (add-new-entity world)
  (lens-set (hash-ref-lens 'entities) world (uuid-string)))

(define (update-world-step w)
  (lens-transform (hash-ref-lens 'step) w add1))

; Returns a new world after running all systems in order
(define (run-all-systems world)
  (let* ([next-world (update-world-step world)]
        [systems (hash-ref next-world 'systems)])
    (foldl (λ (sys w) (sys w)) next-world systems)))

(define (new-component type)
  (hash 'type type))

(define (add-new-system world system)
  (lens-transform
   (hash-ref-lens 'systems)
   world
   (λ (systems) (cons system systems))))

(define (main)
  (~> (new-world)
      add-new-entity
      (add-new-system (λ (w) (begin (print "hi") w)))
      run-all-systems))

(main)