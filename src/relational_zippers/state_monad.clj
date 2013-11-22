(ns relational-zippers.state-monad)


(defmacro do-bind
  ([binding]
     `(do-bind ~binding nil))
  ([binding result]
     (assert (even? (count binding)) "do-bind requires an even number of bindings")
     (let [binds (partition 2 binding)
           state-sym (gensym "state_")
           binds (mapcat
                  (fn [[bind expr]]
                    `[[~bind ~state-sym] (~expr ~state-sym)])
                  binds)]
       `(fn [~state-sym]
          (let ~(vec binds)
            [~result ~state-sym])))))

(defn execute-state [plan]
  (plan {}))


(defn assoc-in-state [path val]
  (fn [state]
    [nil (assoc-in state path val)]))

(defn get-in-state [path]
  (fn [state]
    [(get-in state path)
     state]))

(defn update-in-state [path f & args]
  (fn [state]
    [nil
     (apply update-in state path f args)]))

(defn identity-state [x]
  (println "istate " x)
  (fn [state]
    [x state]))

(defn bind-all [monad-seq]
  (fn [state]
    (reduce
     (fn [[vals state] f]
       (let [[val state] (f state)]
         [(conj vals val) state]))
     [[] state]
     monad-seq)))
