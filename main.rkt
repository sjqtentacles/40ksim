#lang racket

(require threading math lens uuid racket/contract)

(struct component (type attrs) #:transparent)

(struct system (name logic) #:transparent)

(define comp-type? (or/c string? symbol?))

; makes a new empty hashmap for the world state
(define/contract (new-world)
  (-> hash?)
  (hash
   'step 0 ; a world step counter, think of it like time
   'entities (hash) ; Entities hashmap UUIDs to Components
   'components (hash) ; Components hashmap component "types" to entity UUIDs
   'systems null ; Systems list of functions that work on entities/components
   'db null)) ; db is a caching queue for entity addition/removal etc

(define/contract (get-entity world id)
  (-> hash? string? hash?)
  (lens-view (hash-ref-nested-lens 'entities id) world))

(define/contract (entity-exists? world id)
  (-> hash? string? boolean?)
  (hash-has-key? (lens-view (hash-ref-lens 'entities) world) id))

(define/contract (component-exists? world comp-type)
  (-> hash? comp-type? boolean?)
  (hash-has-key? (lens-view (hash-ref-lens 'components) world) comp-type))

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

(define/contract (add-new-entity world id)
  (-> hash? string? hash?)
  (lens-set (hash-ref-lens 'entities) world (hash id null)))

(define/contract (update-world-step w)
  (-> hash? hash?)
  (lens-transform (hash-ref-lens 'step) w add1))

(define/contract (run-all-systems world)
  (-> hash? hash?)
  (let* ([next-world (update-world-step world)]
        [systems (hash-ref next-world 'systems)])
    (foldl (λ (sys w) ((system-logic sys) w)) next-world systems)))

(define/contract (add-new-system world system)
  (-> hash? system? hash?)
  (lens-transform
   (hash-ref-lens 'systems)
   world
   (λ (systems) (cons system systems))))

(define/contract (remove-entity-from-entities world id)
  (-> hash? string? hash?)
  (lens-transform
   (hash-ref-lens 'entities)
   world
   (λ (ents-map) (hash-remove ents-map id))))

(define/contract (remove-component-type-from-components world comp-type)
  (-> hash? comp-type? hash?)
  (lens-transform
   (hash-ref-lens 'components)
   world
   (λ (comps-map) (hash-remove comps-map comp-type))))

(define/contract (remove-components-with-type comps-list comp-type)
  (-> (listof comp-type?)  comp-type? (listof comp-type?))
  (filter (λ (c) (not (equal? (component-type c) comp-type))) comps-list))

(define/contract (remove-components-from-entity-with-type world ent-id comp-type)
  (-> hash? string? comp-type? hash?)
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