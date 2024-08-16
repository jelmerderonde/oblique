(ns oblique.db
  (:require
   [oblique.clock :as clock]
   [me.tonsky.persistent-sorted-set :as pss]
   [oblique.util :refer [by ascending descending]]
   [arrangement.core :as order]))

(defrecord Op [op e a c args])

(defn op
  [op e a c & args]
  (Op. op e a c (vec args)))

(defrecord Datom [e a v c added])

(def cmp-op-eac (by :e order/rank :a order/rank :c clock/by-clock :op order/rank :args order/rank))
(def cmp-datom-eavc (by :e order/rank :a order/rank :v order/rank :c clock/by-clock :added descending))
(def cmp-datom-aevc (by :a order/rank :e order/rank :v order/rank :c clock/by-clock :added descending))
(def cmp-datom-avec (by :a order/rank :v order/rank :e order/rank :c clock/by-clock :added descending))

(defrecord DB [schema ops eavc aevc avec])

(def types
  {:lwwr {:cardinality :one
          :ops #{:reg/set}}
   :lwwset {:cardinality :many
            :ops #{:set/conj :set/disj}}})

;; Improve this
(defn validate-schema
  [schema]
  (and (map? schema)
       (every? (fn [[attr {:keys [obl/type]}]]
                 (and (keyword? attr)
                      (contains? types type)))
               schema)))

(defn multival?
  [db a]
  (let [type (get-in db [:schema a :obl/type])
        cardinality (get-in types [type :cardinality])]
    (= cardinality :many)))

(defn empty-db
  [schema]
  (assert (validate-schema schema))
  (DB. schema
       (pss/sorted-set-by cmp-op-eac)
       (pss/sorted-set-by cmp-datom-eavc)
       (pss/sorted-set-by cmp-datom-aevc)
       (pss/sorted-set-by cmp-datom-avec)))

(def schema
  {:register {:obl/type :lwwr}
   :set {:obl/type :lwwset
         :obl/ref true}})

(defn component-count
  [components]
  (count (take-while some? components)))

(defn components->cmp
  [index & components]
  (assert (#{:eavc :aevc :avec} index))
  (let [orderings {:eavc [:e order/rank :a order/rank :v order/rank :c clock/by-clock]
                   :aevc [:a order/rank :e order/rank :v order/rank :c clock/by-clock]
                   :avec [:a order/rank :v order/rank :e order/rank :c clock/by-clock]}]
    (apply by (take (* (component-count components) 2) (get orderings index)))))

(defn datoms
  [db index c1 c2 c3 c4]
  (assert (#{:eavc :aevc :avec} index))
  (let [datom (case index
                :eavc (Datom. c1 c2 c3 c4 nil)
                :aevc (Datom. c2 c1 c3 c4 nil)
                :avec (Datom. c3 c1 c2 c4 nil))
        cmp (components->cmp index c1 c2 c3 c4)]
    (pss/slice (get db index) datom datom cmp)))

(defn ops
  [db e a c op]
  (let [ordering [:e order/rank :a order/rank :c clock/by-clock :op order/rank]
        op (Op. op e a c nil)
        cmp (apply by (take (* (component-count [e a c op]) 2) ordering))]
    (pss/slice (get db :ops) op op cmp)))

(defmulti process-op (fn [_ op] (.op op)))

(defmethod process-op :obl/retract
  [_ op]
  (let [e (:e op)
        a (:a op)
        c (:c op)
        v (first (:args op))]
    (Datom. e a v c false)))

(defmethod process-op :obl/retract-entity
  [_ op]
  (let [e (:e op)
        c (:c op)]
    (Datom. e :obl/retracted true c true)))

(defmethod process-op :obl/restore-entity
  [_ op]
  (let [e (:e op)
        c (:c op)]
    (Datom. e :obl/retracted true c false)))

;; TODO: rename all ops using :obl ns
(defmethod process-op :reg/set
  [db op]
  (let [e (:e op)
        a (:a op)
        c (:c op)
        ops (ops db e a nil nil)
        v (-> ops last :args first)]
    (Datom. e a v c true)))

;; This one might be incorrect...
;; Unset is maybe not a valid operation on a LWW register.
;; It would set the attribute to nil
;; Instead we should retract it.
(defmethod process-op :reg/unset
  [_ op]
  (let [e (:e op)
        a (:a op)
        c (:c op)
        v (-> op :args first)]
    (Datom. e a v c false)))

(defmethod process-op :set/conj
  [db op]
  (let [e (:e op)
        a (:a op)
        c (:c op)
        ops (ops db e a nil nil)
        v (-> ops last :args first)]
    (Datom. e a v c true)))

(defmethod process-op :set/disj
  [db op]
  (let [e (:e op)
        a (:a op)
        c (:c op)
        ops (ops db e a nil nil)
        v (-> ops last :args first)]
    (Datom. e a v c false)))

;; TODO: change this, based on ref attrs, using the schema.
(defn indexed-attr?
  [db a]
  false)

(defn with-op
  [db op]
  ;; The steps
  ;; 0. Add the op to the db
  ;; 1. Process the op: should result in datoms
  ;; 2. Index the datoms
  ;; 3. Produce report, for subscribe/listen stuff at least.
  ;; Return db
  (let [db' (update db :ops conj op)
        datom (process-op db' op)]
    (cond-> db'
      true (update :eavc conj datom)
      true (update :aevc conj datom)
      true (update :avec conj datom))))

(declare hash-entity equiv-entity lookup-all-entity-attr lookup-entity-attr)

(deftype Entity [db eid]
  ;; Deleted the Object stuff, should probably add it back
  Object
  (toString [e]      (pr-str (lookup-all-entity-attr e)))
  (hashCode [e]      (hash-entity e))
  (equals [e o]      (equiv-entity e o))

  clojure.lang.Seqable
  (seq [e]           (seq (lookup-all-entity-attr e)))

  clojure.lang.Associative
  (equiv [e o]       (equiv-entity e o))
  (containsKey [e k] (not= ::nf (lookup-entity-attr e k ::nf)))
  (entryAt [e k]     (some->> (lookup-entity-attr e k) (clojure.lang.MapEntry. k)))

  (empty [e]         (throw (UnsupportedOperationException.)))
  (assoc [e k v]     (throw (UnsupportedOperationException.)))
  (cons  [e [k v]]   (throw (UnsupportedOperationException.)))
  (count [e]         (count (lookup-all-entity-attr e)))

  clojure.lang.ILookup
  (valAt [e k]       (lookup-entity-attr e k))
  (valAt [e k not-found] (lookup-entity-attr e k not-found))

  clojure.lang.IFn
  (invoke [e k]      (lookup-entity-attr e k))
  (invoke [e k not-found] (lookup-entity-attr e k not-found)))

(defn- hash-entity
  [e]
  (clojure.lang.Util/hashCombine
   (hash (.-eid e))
   (System/identityHashCode (.-db e))))

(defn- equiv-entity [^Entity this that]
  (and
   (instance? Entity that)
   (identical? (.-db this) (.-db ^Entity that)) ; `=` and `hash` on db is expensive
   (= (.-eid this) (.-eid ^Entity that))))

;; Should result in value for the datoms passed in.
(defn entity-attr
  [db a datoms]
  (if (multival? db a)
    (reduce
     (fn [acc part]
       (if (:added (last part))
         (conj acc (:v (last part)))
         acc))
     #{}
     (partition-by :v datoms))
    (when (:added (last datoms))
      (:v (last datoms)))))

(defn lookup-entity-attr
  ([this attr]
   (lookup-entity-attr this attr nil))
  ([^Entity this attr not-found]
   (if (= attr :obl/id)
     (.-eid this)
     (let [db (.-db this)
           datoms (datoms db :eavc (.-eid this) attr nil nil)]
       (if (not-empty datoms)
         (if-let [v (entity-attr db attr datoms)]
           v not-found)
         not-found)))))

(defn lookup-all-entity-attr
  [^Entity e]
  (let [db (.-db e)
        datoms (datoms db :eavc (.-eid e) nil nil nil)]
    (assoc
     (reduce
      (fn [acc part]
        (let [a (:a (first part))]
          (if-let [v (entity-attr db a part)]
            (assoc acc a v)
            acc)))
      {} (partition-by :a datoms))
     :obl/id (.-eid e))))

(defn entity
  [db eid]
  (Entity. db eid))

(let [rid (random-uuid)
      e (random-uuid)
      c0 (clock/init rid)
      c1 (clock/advance c0)
      c2 (clock/advance c1)
      c3 (clock/advance c2)
      c4 (clock/advance c3)
      db (empty-db schema)
      op1 (Op. :reg/set e :register c0 [:hello])
      op2 (Op. :reg/set e :register c1 [:world])
      op3 (Op. :set/conj e :set c2 [:a])
      op4 (Op. :set/conj e :set c3 [:b])
      op5 (Op. :set/disj e :set c4 [:a])
      ops [op1 op2 op3 op4 op5]]
  (-> (reduce with-op db ops)
      (entity e)))

(defn rand-test-datom
  [eids clock]
  (let [c (swap! clock clock/advance)]
    (Datom.
     (rand-nth eids)
     (keyword (str "attr-" (rand-int 30)))
     (rand-int 1000)
     c
     (rand-nth [true false]))))

;; TODO: create db-from-ops fn
#_(def test-db
    (let [clock (atom (clock/init (random-uuid)))
          eids (repeatedly 100 random-uuid)
          datoms (repeatedly 10000 (fn [] (rand-test-datom eids clock)))]
      {:eavc (into (pss/sorted-set-by (by :e ascending :a ascending :v ascending :c clock/by-clock)) datoms)
       :aevc (into (pss/sorted-set-by (by :a ascending :e ascending :v ascending :c clock/by-clock)) datoms)
       :avec (into (pss/sorted-set-by (by :a ascending :v ascending :e ascending :c clock/by-clock)) datoms)}))


(comment
  #_(ns-unmap *ns* 'process-op))