(ns relational-zippers.protocols
  (:refer-clojure :exclude [children parents]))


(defprotocol ICursor
  (data-source [this])
  (id [this])
  (children [this])
  (parents [this]))

(defprotocol IGoto
  (goto [this id]))

(defprotocol IDataSource
  (datoms [id] "returns a seq of eav tuples")
  (pointers-to [this id] "returns a seq of eav tuples where (= id v)")
  (children-of [this id] "returns all the entities that are children of id")
  (query-by-e [this e] "returns a seq of av tuples where (= e e)")
  (query-by-ea [this e a not-found])
  (ids [this] "returns a seq of all ids")

  (new-entity [this id])
  (assert-datom [this id attr val]))
