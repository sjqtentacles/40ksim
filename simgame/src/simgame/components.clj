(ns simgame.components)

(defn component-exists?
  [world type]
  (boolean (get-in world '(:components type))))

(defn remove-component
  [world type]
  (dissoc world :components type))

(defn get-unused-components
  [world]
  (reduce-kv 
    (fn [l k v] 
      (if (= 0 (count v))
        (cons k l) l))
    '()
    world))

(defn remove-unused-components
  [world]
  (let [unused-types (get-unused-components world)]
    (reduce (fn [acc k] (dissoc acc k)) world unused-types)))

;; (struct component (type attrs) #:transparent)

;; (define comp-type? (or/c string? symbol?))

;; (define/contract (add-component-to-entity world comp id)
;;   (-> hash? component? string? hash?)
;;   (let* ([ent-id-lens (hash-ref-nested-lens 'entities id)] ; getting entity uuid's hashmap
;;         [comp-lens (hash-ref-lens 'components)] ; getting components hashmap
;;         [with-updated-components ; inserting new comp into components hashmap
;;             (lens-transform
;;              comp-lens
;;              world
;;              (λ (comps-hash) ; using a hash-update func to do it
;;                (hash-update comps-hash
;;                             (component-type comp)
;;                             (λ (ents) (cons id ents))
;;                             null)))])
;;     (lens-transform ent-id-lens with-updated-components (λ (comps) (cons comp comps)))))

;; (define/contract (location x y z)
;;   (-> integer? integer? integer? component?)
;;   (component 'location (hash 'x x 'y y 'z z)))

;; (define/contract (health amount)
;;   (-> integer? component?)
;;   (component 'health (hash 'value amount)))

;; (define/contract (visible is-visible?)
;;   (-> boolean? component?)
;;   (component 'visible (hash 'visible is-visible?)))
