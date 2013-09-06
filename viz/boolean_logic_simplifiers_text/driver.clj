(ns boolean-logic-simplifiers-text.driver
  "A driver for visualizing the boolean logic simplifiers code as simple unordered lists."
  (:require [taoensso.timbre :as timbre
              :refer (debug info warn error)]
            [clojure.edn]
            [clojure.zip :as z]
            [boolean-logic-simplifiers.core :as simplifiers]
            [boolean-logic-simplifiers.factory :as factory]
            [vdd-core.core :as vdd]
            [vdd-core.capture-global :as capture])
  (:use [clojure.pprint]))

(defn- walk-tree
  "Walks an entire condition tree to force side effects. Returns the condition when it's done"
  [c]
  (let [zipped (factory/cond-zipper c)]
    (loop [zip-node zipped]
      (if (z/end? zip-node)
        (z/root zip-node)
        (recur (z/next zip-node))))))

(defn replace-condition
  "Replaces the condition in the root cond by matching the id"
  [root-cond condition]
  (let [id (:id condition)
        zipped (factory/cond-zipper root-cond)]
    (loop [zip-node zipped]
      (if (z/end? zip-node)
        (throw (Exception. (str "Did not find condition with id " id)))
        (if (-> zip-node z/node :id (= id))
          (-> zip-node (z/replace condition) z/root)
          (recur (z/next zip-node)))))))

(defn- run-at-id
  "Allows running a function on a zipper at a condition within the root-cond with an id. The
  function result will be returned."
  [root-cond id f]
  (loop [zip-node (factory/cond-zipper root-cond)]
    (if (z/end? zip-node)
      (throw (Exception. (str "Did not find condition with id " id)))
      (if (-> zip-node z/node :id (= id))
        (f zip-node)
        (recur (z/next zip-node))))))

(defmulti apply-change
  "Applies a recorded change to the root condition"
  (fn [root-cond change] (:type change)))

(defmethod apply-change :node-remove
  [root-cond {id :id}]
  ; Remove the condition with the id and return the updated root
  (run-at-id root-cond id #(-> % z/remove z/root)))

(defmethod apply-change :node-move
  [root-cond {from :from to :to id :id}]
  (let [condition (run-at-id root-cond id z/node)
        root-cond (apply-change root-cond {:type :node-remove :from from :id id})]
    (run-at-id
      root-cond
      to
      (fn [to-cond-z]
        (let [to-cond (z/node to-cond-z)
              updated (update-in to-cond [:conditions] #(conj % condition))]
          (z/root (z/replace to-cond-z updated)))))))


(defn- test-simplifiers
  [logic-str]
  (debug "Data received:" logic-str)
  (capture/enable)
  (capture/reset-captured!)

  (let [root-cond (factory/string->condition logic-str)
        simplified (walk-tree (simplifiers/simplify root-cond))
        changes (capture/captured)
        versions (reduce (fn [versions change]
                           (conj versions (apply-change (last versions) change)))
                         [root-cond]
                         changes)]
    (vdd/data->viz versions)))
