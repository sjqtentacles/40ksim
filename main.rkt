#lang racket

(require threading math lens uuid)

(struct component (type attrs) #:transparent)

(define (new-world)
  (hash
   'step 0 ; a world step counter, think of it like time
   'entities (make-hash) ; Entities hashmap UUIDs to Components
   'components (make-hash) ; Components hashmap component "types" to entity UUIDs
   'systems null ; Systems list of functions that work on entities/components
   'db null)) ; db is a caching queue for entity addition/removal etc

(define (get-entity world id)
  (lens-view (hash-ref-nested-lens 'entities id) world))

(define (add-component-to-entities world comp id)
  (let ([l (hash-ref-nested-lens 'entities id)])
    (lens-transform l world (λ (comps) (cons comp comps)))))

; Returns a uuid string
(define (add-new-entity world id)
  (lens-set (hash-ref-lens 'entities) world (hash id null)))

(define (update-world-step w)
  (lens-transform (hash-ref-lens 'step) w add1))

; Returns a new world after running all systems in order
(define (run-all-systems world)
  (let* ([next-world (update-world-step world)]
        [systems (hash-ref next-world 'systems)])
    (foldl (λ (sys w) (sys w)) next-world systems)))

(define (add-new-system world system)
  (lens-transform
   (hash-ref-lens 'systems)
   world
   (λ (systems) (cons system systems))))

(define test-uuid (uuid-string))

(define (main)
  (~> (new-world)
      (add-new-entity _ test-uuid)
      (add-component-to-entities _ (component 'spell (hash 'name "fire" 'damage 3)) test-uuid)
      (add-new-system _ (λ (w) (begin (print "hi") w)))
      run-all-systems))

(main)