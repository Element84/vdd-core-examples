(ns boolean-logic-simplifiers.factory-dsl
  "This namespace allows easily creating conditions for testing. Can be called like the following:
    (or
      (and
        (= :x 5)
        (= :z 7))
      (and
        (= :a 1)
        (= :b 2)))
  "
  (:refer-clojure :exclude [and or = < >]))

(defn and [& parts]
  {:type :and :conditions parts})

(defn or [& parts]
  {:type :or :conditions parts})

(defn = [v1 v2]
  {:type :eq :value1 v1 :value2 v2})

(defn < [v1 v2]
  {:type :lt :value1 v1 :value2 v2})

(defn > [v1 v2]
  {:type :gt :value1 v1 :value2 v2})
