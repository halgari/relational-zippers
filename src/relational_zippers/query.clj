(ns relational-zippers.query
  (:refer-clojure :exclude [== children parents])
  (:require [relational-zippers.protocols :as proto]
            [clojure.core.logic :refer :all]
            [clojure.core.logic.protocols :refer [walk]]))


(defn datoms [cursor e a v]
  (fn [sub]
    (let [e (walk sub e)
          a (walk sub a)
          v (walk sub v)]
      (->
       (for [dms (proto/query-by (proto/data-source cursor)
                           [(not (lvar? e))
                            (not (lvar? a))
                            (not (lvar? v))]
                           [e a v])]
         (unify subs [e a v] dms))
       to-stream))))
