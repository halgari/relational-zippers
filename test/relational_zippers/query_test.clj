(ns relational-zippers.query-test
  (:refer-clojure :exclude [children parents ==])
  (:require [clojure.test :refer :all]
            [relational-zippers.protocols :refer :all]
            [relational-zippers.query :as q]
            [relational-zippers.core :refer :all]
            [clojure.core.logic :refer :all :exclude [is]]))


(deftest query-tests
  (is (= (let [source (-> (memory-data-source)
                          (assert-item {:a 42
                                        :b 33
                                        :c {:a 42
                                            :b 33}}))]
           (-> (run* [q]
                     (q/datoms source q :a 42)
                     (q/datoms source q :b 33))
               count))
         2)))


(deftest pattern-matching-tests
  (is (= (let [source (-> (memory-data-source)
                          (assert-item '(if (= x 1)
                                          (+ x 1))))]
           (-> (run* [?q]
                     (q/matches source (if ?q _ _)))))
         nil)))

(run-tests)



(-> (q/emit-pattern-forms '(if ?x _ 1 2))
    (relational-zippers.state-monad/execute-state)
    (q/state->logic-forms #{'?x})
    clojure.pprint/pprint)

(clojure.pprint/pprint (macroexpand '(q/matches 1 (if ?q _ _))))
