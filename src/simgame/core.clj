(ns simgame.core
  (:gen-class)
  (:require 
    [simgame.components :as comps]
    [simgame.entities :as ents]
    [simgame.systems :as syses]))

(defn refresh []
  (use 'simgame.core :reload))

(defn refresh-all []
  (use 'simgame.core :reload-all))

(defn new-world []
  {
    :entities nil
    :components nil
    :systems nil
    :db nil
    :age 0
  })

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))