(ns relational-zippers.core-test
  (:refer-clojure :exclude [children parents])
  (:require [clojure.test :refer :all]
            [relational-zippers.protocols :refer :all]
            [relational-zippers.core :refer :all]))

(defn round-trip [x]
  (-> (memory-data-source)
      (assert-item x)
      export))

(deftest roundtrip-tests
  (let [test-forms [{:a 42}
                    {:a 42
                     :b 43}
                    {:a 42
                     :b {:foo 44}}]]
    (doseq [f test-forms]
      (is (= f (round-trip f))))))

(deftest navigation-tests
  (let [form {:a 42
              :b {:foo 34
                  :bar {:z 33}}}

        start-cursor (-> (memory-data-source)
                         (assert-item form))]
    (is (= (-> start-cursor
               children
               first
               export)
           {:foo 34
            :bar {:z 33}}))

    (is (= (-> start-cursor
               children
               first
               children
               first
               export)
           {:z 33}))

    (is (= (-> start-cursor
               children
               first
               children
               first
               parents
               first
               parents
               first
               export)
           form))))
