#lang racket

(require threading math lens uuid racket/contract)

(struct component (type attrs) #:transparent)

(struct system (name logic) #:transparent)

; makes a new empty hashmap for the world state
(define/contract (new-world)
  (-> hash?)
  (hash
   'step 0 ; a world step counter, think of it like time
   'entities (hash) ; Entities hashmap UUIDs to Components
   'components (hash) ; Components hashmap component "types" to entity UUIDs
   'systems null ; Systems list of functions that work on entities/components
   'db null)) ; db is a caching queue for entity addition/removal etc

; world uuid -> world
(define/contract (get-entity world id)
  (-> hash? string? hash?)
  (lens-view (hash-ref-nested-lens 'entities id) world))

; world uuid -> boolean
(define/contract (entity-exists? world id)
  (-> hash? string? hash?)
  (hash-has-key? (lens-view (hash-ref-lens 'entities) world) id))

; world component uuid -> world
(define/contract (add-component-to-entity world comp id)
  (-> hash? component? string? hash?)
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
(define/contract (add-new-entity world id)
  (-> hash? string? hash?)
  (lens-set (hash-ref-lens 'entities) world (hash id null)))

; world -> world
(define/contract (update-world-step w)
  (-> hash? hash?)
  (lens-transform (hash-ref-lens 'step) w add1))

; Returns a new world after running all systems in order
(define/contract (run-all-systems world)
  (-> hash? hash?)
  (let* ([next-world (update-world-step world)]
        [systems (hash-ref next-world 'systems)])
    (foldl (λ (sys w) ((system-logic sys) w)) next-world systems)))

; world -> (world -> world) -> world
(define/contract (add-new-system world system)
  (-> hash? system? hash?)
  (lens-transform
   (hash-ref-lens 'systems)
   world
   (λ (systems) (cons system systems))))

; world -> uuid -> world
(define/contract (remove-entity-from-entities world id)
  (-> hash? string? hash?)
  (lens-transform
   (hash-ref-lens 'entities)
   world
   (λ (ents-map) (hash-remove ents-map id))))

; world -> component-type -> world
(define/contract (remove-component-type-from-components world comp-type)
  (-> hash? (or/c string? symbol?) hash?)
  (lens-transform
   (hash-ref-lens 'components)
   world
   (λ (comps-map) (hash-remove comps-map comp-type))))

; components list -> component-type -> components list
(define/contract (remove-components-with-type comps-list comp-type)
  (-> (listof (or/c string? symbol?)) (or/c string? symbol?) (listof (or/c string? symbol?)))
  (filter (λ (c) (not (equal? (component-type c) comp-type))) comps-list))

; world -> uuid -> component-type -> world
(define/contract (remove-components-from-entity-with-type world ent-id comp-type)
  (-> hash? string? (or/c string? symbol?) hash?)
  (lens-transform
   (hash-ref-nested-lens 'entities ent-id)
   world
   (λ (comps) (remove-components-with-type comps comp-type))))

(define test-uuid (uuid-string))

(define/contract (main)
  (-> hash?)
  (~> (new-world)
      (add-new-entity _ test-uuid)
      (add-component-to-entity _ (component 'spell (hash 'name "fire" 'damage 3)) test-uuid)
      (add-new-system _ (system "hi-test" (λ (w) (begin (print "hi") w))))
      run-all-systems))

(main)