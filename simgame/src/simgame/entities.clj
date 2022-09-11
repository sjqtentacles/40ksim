(ns simgame.entities)

(defn generate-new-id
  []
  (.toString (java.util.UUID/randomUUID)))

(defn get-entity
  [world id]
  (get-in world :entities id))

(defn entity-exists?
  [world id]
  (boolean (get-entity world id)))

(defn add-new-entity world id comps-list
  [world]
  (assoc-in world [:components id] comps-list))

;; (define/contract (get-entity-components-types world id)
;;   (-> hash? string? (listof comp-type?))
;;   (let* ([l (hash-ref-nested-lens 'entities id)]
;;         [ent-comp-types (map (λ (c) (component-type c)) (lens-view l world))])
;;         (remove-duplicates ent-comp-types)))

;; (define/contract (remove-entity-from-entities world id)
;;   (-> hash? string? hash?)
;;   (lens-transform
;;    (hash-ref-lens 'entities)
;;    world
;;    (λ (ents-map) (hash-remove ents-map id))))

;; (define/contract (remove-entity-from-components world id)
;;   (-> hash? string? hash?)
;;   (let ([l (hash-ref-lens 'components)]
;;         [comps-to-remove-from (get-entity-components-types world id)])
;;     (lens-transform
;;      l
;;      world
;;      (λ (comps-map)
;;        (foldl (λ (comp-type w)
;;                 (lens-transform
;;                  (hash-ref-nested-lens 'components comp-type)
;;                  w
;;                  (λ (ents)
;;                    (filter (λ (e) (not (equal? e id))) ents))))
;;               world
;;               comps-map)))))

;; (define/contract (remove-entity world id)
;;   (-> hash? string? hash?)
;;   (~> world
;;       (remove-entity-from-components id)
;;       (remove-entity-from-entities id)))

;; (define/contract (remove-components-from-entity-with-type world ent-id comp-type)
;;   (-> hash? string? comp-type? hash?)
;;   (lens-transform
;;    (hash-ref-nested-lens 'entities ent-id)
;;    world
;;    (λ (comps) (remove-components-with-type comps comp-type))))