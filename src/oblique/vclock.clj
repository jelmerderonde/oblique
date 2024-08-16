(ns oblique.vclock
  (:refer-clojure :exclude [inc]))

(defn timestamp-in-seconds
  []
  (Math/round (double (/ (.getTime (java.util.Date.)) 1000))))

;; A timestamp in Clock A is said to be before another a timestamp in Clock B if:

;;     All vector values in clock A are less than or equal to clock B
;;     At least one value in clock A is less than the corresponding value in Clock B


(defrecord Clock [pid counters ts]
  Comparable
  (compareTo [this other]
             (let [pids (into (set (keys counters)) (keys (:counters other)))
                   comparisons (for [pid pids]
                                 (let [this-c (get counters pid 0)
                                       other-c (get (:counters other) pid 0)]
                                   (.compareTo this-c other-c)))] 
               (cond
                 (and (some neg? comparisons)
                      (every? (complement pos?) comparisons))
                 -1

                 (and (some pos? comparisons)
                      (every? (complement neg?) comparisons))
                 1

                 :else (.compareTo pid (:pid other))))))

(defn init
  ([]
   (init (random-uuid)))
  ([pid]
   (Clock. pid {pid 0} (timestamp-in-seconds))))

(defn inc
  [clock]
  (-> clock
      (update-in [:counters (:pid clock)] clojure.core/inc)
      (assoc :ts (timestamp-in-seconds))))

(defn- merge-counter
  [this-c other-c]
  (if (:counters this-c) this-c other-c))

(defn receive
  [clock other-clock]
  (let [clock' (inc clock)
        counters' (merge-with merge-counter (:counters clock') (:counters other-clock))]
    (assoc clock' :counters counters')))

;; (defn concurrent?
;;   "Compares clocks to check if they are concurrent.
;;    Does not use the pid to break ties."
;;   [this other])









