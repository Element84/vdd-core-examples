(ns boolean-logic-simplifiers.test.core
  (:require [boolean-logic-simplifiers.factory :as f]
            [boolean-logic-simplifiers.factory-dsl :as fd])
  (:use [boolean-logic-simplifiers.core]
        [clojure.test]))

(defn v
  "Short helper to create a value condition."
  [v]
  (fd/= :v v))

; None of these conditions should be changed by the and simplifier
(deftest test-and-simplifier-none-required
  (are [c] (= c (and-simplify c))
       (v 1)
       (fd/or (v 1) (v 2))
       (fd/and (v 1) (v 2))
       (fd/and (v 1) (fd/or (v 2) (v 3)))))

(deftest test-and-simplifier-within-or
  (is (= (fd/or
           (v 4)
           (fd/or
             (v 6)
             (fd/and
               (v 1)
               (v 2)
               (v 3))))
         (and-simplify
           (fd/or
             (v 4)
             (fd/or
               (v 6)
               (fd/and
                 (v 1)
                 (fd/and
                   (v 2)
                   (v 3)))))))))

(deftest test-and-simplifier-single-level
  (is (= (fd/and
           (v 1)
           (v 2)
           (v 3))
         (and-simplify
           (fd/and
             (v 1)
             (fd/and
               (v 2)
               (v 3)))))))

(deftest test-and-simplifier-many-nested
  (is (= (fd/and
           (v 1)
           (v 2)
           (v 3)
           (v 4))
         (and-simplify
           (fd/and
             (v 1)
             (fd/and
               (v 2)
               (fd/and
                 (v 3)
                 (v 4))))))))
