(ns boolean-logic-simplifiers.factory
  (:require [clojure.zip :as z]
            [boolean-logic-simplifiers.factory-dsl])
  (:import [java.io StringWriter]))

(defn cond-zipper
  "Returns a clojure zipper object that can traverse over a condition"
  [cond]
  (z/zipper
    :conditions
    :conditions
    (fn [existing new-children]
      (assoc existing :conditions new-children))
    cond))

(defn- assign-ids
  "Assigns unique ids to the condition and its children."
  [cond]
  (let [zipped (cond-zipper cond)]
    (loop [zipped zipped next-id 0]
      (if (z/end? zipped)
        (z/root zipped)
        (let [next-node (z/replace zipped (-> zipped
                                                 z/node
                                                 (assoc :id next-id)))]
          (recur (z/next next-node) (inc next-id)))))))

(defn string->condition
  "Converts a string containing s-expressions of a boolean tree into an actual condition tree."
  [s]
  (binding [*ns* (find-ns 'boolean-logic-simplifiers.factory-dsl)]
    (assign-ids (load-string s))))
