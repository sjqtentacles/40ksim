(ns simgame.systems)

(defn add-new-system
  [world sys]
  (update-in world [:systems] (fn [syses] (cons sys syses))))

(defn remove-systems-by-name
  [world name]
  (update-in 
    world 
    [:systems] 
    (fn [syses] 
      (filter
        (fn [s] 
          (let [n (get s :name)]
            (not= n name)))
        syses))))

(defn get-systems-by-name
  [world name]
  (filter (fn [s] (= name (get s :name))) (get world :systems)))

(defn run-systems-by-name
  [world name]
  (reduce (fn [w sys] (sys w)) world (get-systems-by-name world name)))

(defn run-all-systems
  [world]
  (reduce (fn [w sys] (sys w)) world (get world :systems)))

(defn remove-all-systems
  [world]
  (assoc world :systems nil))