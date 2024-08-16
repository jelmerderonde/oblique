(ns oblique.clock
  (:require
   [clojure.string :as str]
   [oblique.util :refer [by ascending descending]]
   [clojure.pprint :refer [cl-format]]
   [criterium.core :as crit]))

;; https://adamwulf.me/2021/05/distributed-clocks-and-crdts/
;; https://jaredforsyth.com/posts/hybrid-logical-clocks/

;; Hybrid logical clocks build upon the foundation of the Lamport clock, and add in a wall clock component as well.
;; This clock has three components: a wall clock time, a counter, and the clock’s unique identifier.
;; In the exceptionally rare instance that two clock’s wall time and counter match, the identifier will break the tie.

;; Here’s how the clock works:

;;     Initialize the clock with the current UTC timestamp and a counter of zero
;;     Whenever an event happens:
;;         If the current wall clock time is after the current HLC timestamp, then update the timestamp and reset the counter to zero
;;         If the current wall clock time is before or equal to the current HLC timestamp, leave the timestamp unchanged and increment the counter
;;     Whenever an event is received:
;;         if the local wall clock time is larger than both our HLC and the event’s HLC, then use that and reset the counter to 0, otherwise, the wall clock is at or before our time, so we can ignore it
;;         if our HLC timestamp is equal to the event’s HLC timestamp, set our counter to one more than the max of both our counters
;;         if our HLC timestamp is after the event’s HLC timestamp, keep our timestamp and increment our counter
;;         last, if the event’s HLC timestamp is larger than our timestamp, use it’s timestamp and set our counter to 1 larger than its count

(defn init
  ([]
   (init (random-uuid)))
  ([replica]
   {:ts (System/currentTimeMillis)
    :count 0
    :replica replica}))

(defn advance
  [clock]
  (let [now (System/currentTimeMillis)]
    (if (> now (:ts clock))
      (assoc clock :ts now :count 0)
      (update clock :count inc))))

(defn receive
  "Returns an updated local clock."
  [local remote]
  (let [now (System/currentTimeMillis)]
    (cond
      ;; now is later than both local and remote
      (and (> now (:ts local))
           (> now (:ts remote)))
      (assoc local :ts now :count 0)

      ;; One or both of local and remote timestamps are in the future

      ;; local and remote timestamp are in the future, but identical
      (= (:ts local) (:ts remote))
      (assoc local :count (inc (max (:count local) (:count remote))))

      ;; Our local timestamp is bigger than the remote one
      (> (:ts local) (:ts remote))
      (update local :count inc)

      ;; The remote timestamp is bigger than our local one
      :else
      (assoc local :ts (:ts remote) :count (inc (:count remote))))))

(defn rank
  "Compare two clocks"
  [one two]
  (let [tsc (compare (:ts one) (:ts two))]
    (if (zero? tsc)
      (let [countc (compare (:count one) (:count two))]
        (if (zero? countc)
          (compare (:replica one) (:replica two))
          countc))
      tsc)))

(def by-clock (by :ts ascending :count ascending :replica ascending))

(defn pack
  [clock]
  (cl-format nil "~36,12'0R:~36,5'0R:~d" (:ts clock) (:count clock) (:replica clock)))

(defn unpack
  [packed-clock]
  (let [[pts pcount preplica] (str/split packed-clock #":")]
    {:ts (Long/parseLong pts 36)
     :count (Integer/parseInt pcount 36)
     :replica (Integer/parseInt preplica)}))

(comment
  (require '[criterium.core :as crit])



  (crit/with-progress-reporting
    (crit/quick-bench
     (let [clock (atom (init 123))
           advance-clock (fn [] (swap! clock advance))
           clocks (take 1000 (repeatedly advance-clock))
           packed (map pack clocks)
           shuffled (shuffle packed)
           sorted (sort shuffled)
           unpacked (map unpack sorted)]
       (= clocks unpacked)) :verbose))
  
  )