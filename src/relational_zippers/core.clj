(ns relational-zippers.core
  (:require [relational-zippers.protocols :refer :all])
  (:refer-clojure :exclude [children parents]))

(def assert-item nil)

(defn debug [x]
  (println x)
  x)

(defmulti assert-item (fn [source x]
                        (cond
                         (map? x) :map
                         (vector? x) :vector
                         (seq? x) :seq)))

(defn sub-entity? [x]
  (or (map? x)
      (vector? x)
      (seq? x)))



(def next-id (atom 100000))

(deftype DBID [name]
  clojure.lang.Named
  (getName [this]
    (.-name this)))

(defmethod print-method DBID
  [v ^java.io.Writer w]
  (.write w (str "<" (name v) ">")))

(defn make-id []
  (DBID. (str "ID-" (swap! next-id inc))))


(deftype Cursor [id source]
  ICursor
  (data-source [this]
    source)
  (id [this] (.-id this))
  (children [this]
    (map (partial goto this)
         (children-of source id)))

  (parents [this]
    (map (partial goto this)
         (pointers-to source id)))

  (entity-type [this]
    (type-of-entity source id))

  IGoto
  (goto [this id]
    (goto source id))

  clojure.lang.Counted
  (count [this]
    (count (query-by-e source id)))

  clojure.lang.Associative
  (assoc [this k v]
    (Cursor. id
             (assert-datom source id k v)))

  clojure.lang.Seqable
  (seq [this]
    (for [[attr val] (query-by-e source id)
          :let [val (if (instance? DBID val)
                    (goto this val)
                    val)]]
      (clojure.lang.MapEntry. attr val)))

  clojure.lang.ILookup
  (valAt [this key]
    (.valAt this key nil))
  (valAt [this key not-found]
    (if (= key :db/id)
      id
      (let [v (query-by-ea source id key not-found)]
        (if (instance? DBID v)
          (goto this v)
          v)))))


(defrecord Entity [])

(deftype VectorEntity [itms orig-seq?]
  IVectorEntity
  (originally-seq? [this] orig-seq?)

  clojure.lang.Seqable
  (seq [this]
    (map-indexed
     (fn [idx val]
       (clojure.lang.MapEntry. idx val))
     itms))

  clojure.lang.ILookup
  (valAt [this x]
    (.valAt this x nil))
  (valAt [this x not-found]
    (get itms x not-found))

  clojure.lang.Counted
  (count [this]
    (count itms))

  clojure.lang.Associative
  (assoc [this k v]
    (VectorEntity. (assoc itms k v) orig-seq?)))

(defrecord DataSource [ents]
  IDataSource
  (datoms [this]
    (for [[id ent] ents
          [attr val] ent]
      [id attr val]))
  (query-by-e [this id]
    (let [e (get ents id)]
      (if (instance? VectorEntity e)
        (seq e)
        (seq (get ents id)))))
  (query-by-ea [this e a not-found]
    (get (get ents e) a not-found))


  (query-by [this mask [e' a' v' :as vals]]
    (println mask vals)
    (debug (condp = mask
             [true true true] (let [ent (get ents e')]
                                (if (and (contains? ent a')
                                         (= (get ent a') v'))
                                  [vals]
                                  []))

             [true false false] (for [[id ent] ents
                                      :when (= id e')
                                      [a v] ent]
                                  [id a v])

             [false true true] (for [[id ent] ents
                                     [a v] ent
                                     :when (= a a')
                                     :when (= v v')
                                     ]
                                 [id a v]))))

  (entity-size [this id]
    (let [ent (get ents id)]
      (count (seq ent))))

  (new-entity [this id]
    (assert (not (get ents id)) (str "Entity " id " already exists"))
    (assoc-in this [:ents id] (->Entity)))

  (new-vector-entity [this id orig-seq?]
    (assert (not (get ents id)) (str "Entity " id " already exists"))
    (assoc-in this [:ents id] (VectorEntity. [] orig-seq?)))

  (children-of [this id]
    (set (for [[_ val] (get ents id)
               :when (instance? DBID val)]
           val)))

  (type-of-entity [this id]
    (let [ent (get ents id)]
      (cond
       (instance? Entity ent) :map

       (and (instance? VectorEntity ent)
            (not (originally-seq? ent))) :vector

       (and (instance? VectorEntity ent)
            (originally-seq? ent)) :seq)))

  (pointers-to [this id]
    (for [[e ent] ents
          [attr val] ent
          :when (identical? val id)]
      e))

  (assert-datom [this e attr val]
    (assert (get ents e) (str "Entity" e " does not exist"))

    (if (sub-entity? val)
      (let [cursor (assert-item this val)
            cid (id cursor)
            source (data-source cursor)]
        (assert-datom source e attr cid))

      (assoc-in this [:ents e attr] val)))

  (cons-datom [this e val]
    (if (sub-entity? val)
      (let [cursor (assert-item this val)
            cid (id cursor)
            source (data-source cursor)]
        (cons-datom source e cid))
      (let [size (count (ents e))]
        (assoc-in this [:ents e size] val))))

  IGoto
  (goto [this id]
    (assert (get ents id) (str "Unknown id: " id))
    (Cursor. id this)))



(defmethod print-method Cursor [itm ^java.io.Writer w]
  (let [r (into {:db/id (id itm)}
                (seq itm))]
    (.write w (str r))))

(defmethod assert-item :map
  [source itm]
  (let [new-id (make-id)]
    (-> (reduce
         (fn [source [k v]]
           (assert-datom source new-id k v))
         (new-entity source new-id)
         itm)
        (goto new-id))))

(defmethod assert-item :vector
  [source itm]
  (let [new-id (make-id)]
    (-> (reduce
         (fn [source v]
           (cons-datom source new-id v))
         (new-vector-entity source new-id false)
         itm)
        (goto new-id))))

(defmethod assert-item :seq
  [source itm]
  (let [new-id (make-id)]
    (-> (reduce
         (fn [source v]
           (cons-datom source new-id v))
         (new-vector-entity source new-id true)
         itm)
        (goto new-id))))



(defmulti export type)

(defmethod export :default
  [x]
  x)

(defmethod export Cursor
  [x]
  (case (entity-type x)
    :map
    (zipmap (keys x)
            (map export (vals x)))
    :vector (vec (map export (vals x)))
    :seq (map export (vals x))))


(defn memory-data-source []
  (DataSource. {}))

(comment

  (-> (assert-item (DataSource. {}) {:foo 1 :bar 2 :m {:zoo 44}})
      (assoc-in [:cool :m :r :foo] 433)
      export
      #_data-source
      #_(datoms)

      #_(get :bar)))
