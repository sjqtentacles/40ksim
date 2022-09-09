#lang racket

(require
  lens
  "components.rkt")

(provide (all-defined-out))

(define/contract (get-entity world id)
  (-> hash? string? hash?)
  (lens-view (hash-ref-nested-lens 'entities id) world))

(define/contract (entity-exists? world id)
  (-> hash? string? boolean?)
  (hash-has-key? (lens-view (hash-ref-lens 'entities) world) id))

(define/contract (add-new-entity world id)
  (-> hash? string? hash?)
  (lens-set (hash-ref-lens 'entities) world (hash id null)))

(define/contract (get-entity-components-types world id)
  (-> hash? string? (listof comp-type?))
  (let* ([l (hash-ref-nested-lens 'entities id)]
        [ent-comp-types (map (λ (c) (component-type c)) (lens-view l world))])
        (remove-duplicates ent-comp-types)))

(define/contract (remove-entity-from-entities world id)
  (-> hash? string? hash?)
  (lens-transform
   (hash-ref-lens 'entities)
   world
   (λ (ents-map) (hash-remove ents-map id))))

(define/contract (remove-components-from-entity-with-type world ent-id comp-type)
  (-> hash? string? comp-type? hash?)
  (lens-transform
   (hash-ref-nested-lens 'entities ent-id)
   world
   (λ (comps) (remove-components-with-type comps comp-type))))