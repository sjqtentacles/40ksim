#lang racket

(require
  threading
  math
  lens
  uuid
  racket/contract
  "components.rkt"
  "entities.rkt"
  "systems.rkt")

; makes a new empty hashmap for the world state
(define/contract (new-world)
  (-> hash?)
  (hash
   'step 0 ; a world step counter, think of it like time
   'entities (hash) ; Entities hashmap UUIDs to Components
   'components (hash) ; Components hashmap component "types" to entity UUIDs
   'systems null ; Systems list of functions that work on entities/components
   'db null)) ; db is a caching queue for entity addition/removal etc

(define/contract (update-world-step w)
  (-> hash? hash?)
  (lens-transform (hash-ref-lens 'step) w add1))

(define/contract (run-all-systems world)
  (-> hash? hash?)
  (let* ([next-world (update-world-step world)]
        [systems (hash-ref next-world 'systems)])
    (foldl (λ (sys w) ((system-logic sys) w)) next-world systems)))

(define test-uuid (uuid-string))

(define/contract (main)
  (-> hash?)
  (~> (new-world)
      (add-new-entity _ test-uuid)
      (add-component-to-entity _ (component 'spell (hash 'name "fire" 'damage 3)) test-uuid)
      (add-component-to-entity (health 100) test-uuid)
      (add-new-system _ (system "hi-test" (λ (w) (begin (print "hi") w))))
      run-all-systems))

(main)