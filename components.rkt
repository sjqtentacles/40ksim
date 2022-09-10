#lang racket

(require lens)

(provide (all-defined-out))

(struct component (type attrs) #:transparent)

(define comp-type? (or/c string? symbol?))

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

(define/contract (remove-component-type-from-components world comp-type)
  (-> hash? comp-type? hash?)
  (lens-transform
   (hash-ref-lens 'components)
   world
   (λ (comps-map) (hash-remove comps-map comp-type))))

(define/contract (remove-components-with-type comps-list comp-type)
  (-> (listof comp-type?)  comp-type? (listof comp-type?))
  (filter (λ (c) (not (equal? (component-type c) comp-type))) comps-list))

(define/contract (location x y z)
  (-> integer? integer? integer? component?)
  (component 'location (hash 'x x 'y y 'z z)))

(define/contract (health amount)
  (-> integer? component?)
  (component 'health (hash 'value amount)))

(define/contract (visible is-visible?)
  (-> boolean? component?)
  (component 'visible (hash 'visible is-visible?)))

(define/contract (remove-unused-components world)
  (-> hash? hash?)
  (let* ([l (hash-ref-lens 'components)]
        [comps (lens-view l world)]
        [mapped-lengths (hash-map comps (λ (k v)
                                          (k . (length v))))]
        [empty-components (filter (λ (kv-pair)
                                    (equal? 0 (cdr kv-pair))) mapped-lengths)]
        [empty-comp-types (hash-keys empty-components)])
    (foldl (λ (t h)
             (lens-transform l h (λ (cs)
                                   (hash-remove cs t)))) ; here we update the comps hash
           world
           empty-comp-types)))