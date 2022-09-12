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

(defn add-new-entity
  [world id comps-list]
  (assoc-in world [:components id] comps-list))

(defn add-component
  [world id comp]
  (def with-updated-entities
    (update-in world [:entities id] (fn [comps] (cons comp comps))))
  (update-in 
    with-updated-entities 
    [:components (get comp :type)] 
    (fn [ids] 
      (cons id ids))))

(defn get-entity-components-types
  [world id]
  (map (fn [comp] (get comp :type)) (get-in world [:entities id])))

(defn remove-entity
  [world id]
  (let [w (dissoc world :entities id)]
    (reduce-kv 
      (fn [m k v] 
        (update m k (filter (fn [e] (not= e id)) v)))
      {}
      w)))

(defn remove-components-from-entity-with-type
  [world id type]
  (update-in 
    world 
    [:entities id] 
    (fn [comps]
      (filter [comp] 
        (not= (get comp :type) type)
        comps))))