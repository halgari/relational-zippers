(ns relational-zippers.protocols
  (:refer-clojure :exclude [children parents]))


(defprotocol ICursor
  (data-source [this])
  (id [this])
  (children [this])
  (parents [this])
  (entity-type [this]))

(defprotocol IGoto
  (goto [this id]))

(defprotocol IVectorEntity
  (originally-seq? [this]))

(defprotocol IDataSource
  (datoms [id] "returns a seq of eav tuples")
  (pointers-to [this id] "returns a seq of eav tuples where (= id v)")
  (children-of [this id] "returns all the entities that are children of id")
  (query-by [this mask vals])
  (query-by-e [this e] "returns a seq of av tuples where (= e e)")
  (query-by-ea [this e a not-found])
  (entity-size [this id])
  (ids [this] "returns a seq of all ids")
  (type-of-entity [this id] "returns :vector :map, etc based on the type of the entity")

  (new-entity [this id])
  (new-vector-entity [this id orig-seq?])
  (assert-datom [this id attr val])
  (cons-datom [this id val]))
