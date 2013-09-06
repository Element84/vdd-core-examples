(ns boolean-logic-simplifiers.driver
  "A driver for visualizing the boolean logic simplifiers code."
  (:require [taoensso.timbre :as timbre
              :refer (debug info warn error)]
            [clojure.edn]
            [clojure.zip :as z]
            [boolean-logic-simplifiers.core :as simplifiers]
            [boolean-logic-simplifiers.factory :as factory]
            [vdd-core.core :as vdd]
            [vdd-core.capture-global :as capture])
  (:use [clojure.pprint]))

(defmulti cond->d3
  "Multimethod to convert a condition into a d3 node."
  (fn [condition] (:type condition)))

(defn make-group-d3-node [{id :id conditions :conditions type :type} title]
  {:name title
   :_id id
   :type type
   :children (map cond->d3 conditions)})

(defmethod cond->d3 :and
  [c]
  (make-group-d3-node c "&&"))

(defmethod cond->d3 :or
  [c]
  (make-group-d3-node c "||"))

(defmethod cond->d3 :eq
  [{v1 :value1 v2 :value2 id :id}]
  {:name (format "%s = %s" v1 v2)
   :_id id
   :type :eq})

(defmethod cond->d3 :gt
  [{v1 :value1 v2 :value2 id :id}]
  {:name (format "%s > %s" v1 v2)
   :_id id
   :type :eq})

(defmethod cond->d3 :lt
  [{v1 :value1 v2 :value2 id :id}]
  {:name (format "%s < %s" v1 v2)
   :_id id
   :type :eq})


(defn- walk-tree
  "Walks an entire condition tree to force side effects. Returns the condition when it's done"
  [c]
  (let [zipped (factory/cond-zipper c)]
    (loop [zip-node zipped]
      (if (z/end? zip-node)
        (z/root zip-node)
        (recur (z/next zip-node))))))

(defn test-simplifiers
  [logic-str]
  (debug "Data received:" logic-str)
  (capture/enable)
  (capture/reset-captured!)
  (let [root-cond (factory/string->condition logic-str)
        simplified (walk-tree (simplifiers/simplify root-cond))]

    (vdd/data->viz {:root (cond->d3 root-cond)
                    :changes (capture/captured)
                    :simplified simplified})))

