#lang racket

(require lens)

(provide (all-defined-out))

(struct system (name logic) #:transparent)

(define/contract (add-new-system world system)
  (-> hash? system? hash?)
  (lens-transform
   (hash-ref-lens 'systems)
   world
   (λ (systems) (cons system systems))))