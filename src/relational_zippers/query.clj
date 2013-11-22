(ns relational-zippers.query
  (:refer-clojure :exclude [== children parents])
  (:require [relational-zippers.protocols :as proto]
            [relational-zippers.state-monad :refer :all]
            [clojure.core.logic :refer :all]
            [clojure.core.logic.protocols :refer [walk]]
            [clojure.set :refer [difference]]))




(defn datoms [cursor e a v]
  (fn [subs]
    (let [e (walk subs e)
          a (walk subs a)
          v (walk subs v)]
      (->
       (for [dms (proto/query-by (proto/data-source cursor)
                           [(not (lvar? e))
                            (not (lvar? a))
                            (not (lvar? v))]
                           [e a v])]
         (do
           (println "compare" [e a v] dms (type  e) (type dms))
           (unify subs [e a v] dms)))
       to-stream))))


(defmulti emit-pattern-forms (fn [x]
                               (cond
                                (seq? x) :seq)))

(defn lvar-symbol? [x]
  (and (symbol? x)
       (.startsWith (name x) "?")))

(defn add-lvar
  ([]
     (let [lvar-sym (gensym "tmp_lvar")]
       (do-bind
        [_ (update-in-state [:fresh-lvars] (fnil conj #{}) lvar-sym)]
        lvar-sym)))
  ([x]
     (do-bind
      [_ (update-in-state [:fresh-lvars] (fnil conj #{}) x)]
      x)))

(defn add-datoms-clause [e a v]
  (do-bind
   [v (update-in-state [:clauses] (fnil conj [])
                       `[~e ~a ~v])]))

(defn post-process-query [forms]
  (map (fn [[f & rest :as form]]
         (if (vector? form)
           `(datoms ~'?db ~@form)
           `(~f ~'?db ~@rest)))
       forms))

(defn state->logic-forms [[_ {:keys [clauses fresh-lvars]}] dont-capture]
  (let [clauses (post-process-query clauses)
        fresh-lvars (difference fresh-lvars dont-capture)]
    `(fresh [~@fresh-lvars]
            ~@clauses)))

(defmethod emit-pattern-forms :default
  [x]
  (println x)
  (cond
   (lvar-symbol? x) (add-lvar x)

   (= x '_) (add-lvar)

   (symbol? x) (identity-state `(symbol ~(name x)))

   :else (do-bind [] x)))

(defmethod emit-pattern-forms :seq
  [x]
  (do-bind
   [f (add-lvar)
    #__ #_(update-in-state [:clauses]
                       (fnil conj [])
                       `(== (proto/entity-size ~'?db ~(count x))))
    _ (bind-all (map-indexed
            (fn [idx itm]
              (do-bind
               [v (emit-pattern-forms itm)
                _ (add-datoms-clause f idx v)]))
            x))]
   f))

(defmacro matches
  ([source form]
     (let [dont-capture (set (keys &env))]
       `(let [~'?db ~source]
               ~(-> (emit-pattern-forms form)
                   execute-state
                   (state->logic-forms dont-capture))))))
