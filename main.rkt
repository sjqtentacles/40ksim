#lang racket

(require threading math lens uuid)

(struct component (type attrs) #:transparent)

(struct system (name logic) #:transparent)

; makes a new empty hashmap for the world state
(define (new-world)
  (hash
   'step 0 ; a world step counter, think of it like time
   'entities (hash) ; Entities hashmap UUIDs to Components
   'components (hash) ; Components hashmap component "types" to entity UUIDs
   'systems null ; Systems list of functions that work on entities/components
   'db null)) ; db is a caching queue for entity addition/removal etc

; world uuid -> world
(define (get-entity world id)
  (lens-view (hash-ref-nested-lens 'entities id) world))

; world uuid -> boolean
(define (entity-exists? world id)
  (hash-has-key? (lens-view (hash-ref-lens 'entities) world) id))

; world component uuid -> world
(define (add-component-to-entity world comp id)
  (let* ([ent-id-lens (hash-ref-nested-lens 'entities id)] ; getting entity uuid's hashmap
        [comp-lens (hash-ref-lens 'components)] ; getting components hashmap
        [with-updated-components ; inserting new comp into components hashmap
            (lens-transform
             comp-lens
             world
             (λ (comps-hash) ; using a hash-update func to do it
               (hash-update comps-hash
                            (component-type comp)
                            (λ (ents) (cons id ents))
                            null)))])
    (lens-transform ent-id-lens with-updated-components (λ (comps) (cons comp comps)))))

; world uuid -> world
(define (add-new-entity world id)
  (lens-set (hash-ref-lens 'entities) world (hash id null)))

; world -> world
(define (update-world-step w)
  (lens-transform (hash-ref-lens 'step) w add1))

; Returns a new world after running all systems in order
(define (run-all-systems world)
  (let* ([next-world (update-world-step world)]
        [systems (hash-ref next-world 'systems)])
    (foldl (λ (sys w) ((system-logic sys) w)) next-world systems)))

; world -> (world -> world) -> world
(define (add-new-system world system)
  (lens-transform
   (hash-ref-lens 'systems)
   world
   (λ (systems) (cons system systems))))

; world -> uuid -> world
(define (remove-entity-from-entities world id)
  (lens-transform
   (hash-ref-lens 'entities)
   world
   (λ (ents-map) (hash-remove ents-map id))))

; world -> component-type -> world
(define (remove-component-type-from-components world comp-type)
  (lens-transform
   (hash-ref-lens 'components)
   world
   (λ (comps-map) (hash-remove comps-map comp-type))))

; components list -> component-type -> components list
(define (remove-components-with-type comps-list comp-type)
  (filter (λ (c) (not (equal? (component-type c) comp-type))) comps-list))

; world -> uuid -> component-type -> world
(define (remove-components-from-entity-with-type world ent-id comp-type)
  (lens-transform
   (hash-ref-nested-lens 'entities ent-id)
   world
   (λ (comps) (remove-components-with-type comps comp-type))))

(define test-uuid (uuid-string))

(define (main)
  (~> (new-world)
      (add-new-entity _ test-uuid)
      (add-component-to-entity _ (component 'spell (hash 'name "fire" 'damage 3)) test-uuid)
      (add-new-system _ (system "hi-test" (λ (w) (begin (print "hi") w))))
      run-all-systems))

(main)