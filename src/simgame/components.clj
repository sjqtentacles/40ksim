(ns simgame.components)

(defn component-exists?
  [world type]
  (boolean (get-in world '(:components type))))

(defn remove-component-from-world
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

(defn remove-all-components
  [world]
  (let [with-empty-comps (assoc world :components {})]
    (reduce-kv (fn [m k v] (assoc m k nil)) {} with-empty-comps)))