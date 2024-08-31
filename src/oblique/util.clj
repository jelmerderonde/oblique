(ns oblique.util)

(defn by
  "Creates a comparator function for sorting collections based on multiple keys
   and orderings. Returns a function that takes two arguments and compares them
   using the provided keys and orderings.

   The returned function compares items based on the specified keys in order.
   If items are equal on one key, it moves to the next key until a difference
   is found or all keys have been exhausted.

   keys-orderings is a sequence of alternating key functions and comparison
   functions:
     key - a function that extracts a value from the items being compared
     ordering - a function that compares two values and returns a number:
                negative if the first value should be sorted before the second,
                positive if after, and zero if they're considered equal for this key"
  [& keys-orderings]
  (fn [a b]
    (loop [[key ordering & keys-orderings] keys-orderings]
      (let [order (ordering (key a) (key b))]
        (if (and (zero? order) keys-orderings)
          (recur keys-orderings)
          order)))))

(defn ascending
  [a b]
  (. clojure.lang.Util compare a b))

(defn descending
  [a b]
  (. clojure.lang.Util compare b a))