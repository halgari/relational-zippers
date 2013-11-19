(ns relational-zippers.core
  (:require [relational-zippers.protocols :refer :all])
  (:refer-clojure :exclude [children parents]))

(def assert-item nil)

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

(defrecord DataSource [ents]
  IDataSource
  (datoms [this]
    (for [[id ent] ents
          [attr val] ent]
      [id attr val]))
  (query-by-e [this id]
    (seq (get ents id)))
  (query-by-ea [this e a not-found]
    (get (get ents e) a not-found))
  (new-entity [this id]
    (assert (not (get ents id)) (str "Entity " id " already exists"))
    (assoc-in this [:ents id] (->Entity)))

  (children-of [this id]
    (set (for [[_ val] (get ents id)
               :when (instance? DBID val)]
           val)))

  (pointers-to [this id]
    (for [[e ent] ents
          [attr val] ent
          :when (identical? val id)]
      val))

  (assert-datom [this e attr val]
    (assert (get ents e) (str "Entity" e " does not exist"))

    (if (sub-entity? val)
      (let [cursor (assert-item this val)
            cid (id cursor)
            source (data-source cursor)]
        (assert-datom source e attr cid))

      (assoc-in this [:ents e attr] val)))

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


(-> (assert-item (DataSource. {}) {:foo 1 :bar 2 :m {:zoo 44}})
    (assoc-in [:cool :m :r :foo] 433)

    (children)
    first
    parents
    first
    #_data-source
    #_(datoms)

    #_(get :bar))
