(ns sprout.my-oo
  (:use clojure.set)
)

(defn objectify
  "Turn a map into a function which can be invoked with prefixed keywords as method names
   The following keywords have special meanings:
   :map when invoked, return the original map, or look up a value if argument is provided
   :non-key in the original map, provides a function to be invoked on non-key arguments
  All provided functions are passed a 'this' as their first argument, so they can easily invoke each other"
  [m] (let [
    fn-ref (ref 0)
    this #(apply @fn-ref %&)
    non-key (m :non-key)
    functionize (fn [f] (if (fn? f) f (constantly f)))
    functionize-map (fn [m] (into {} (for [[k v] m] [k (functionize v)])))
    m+ (assoc (functionize-map m) :map (fn ([_] m) ([_ key] (m key))))
    the-fn (fn ([] (non-key this))
    ([key & rest]
      (let [value (m+ key)]
      (if value (if (fn? value) (apply value this rest) value)
        (apply non-key this key rest)
    )))) 	           
    ] (dosync (ref-set fn-ref the-fn))
    this
))

(defn inherit 
  "Using the given object as a prototype, 'inherit' from it with new members and
  methods, which can override the existing ones."
  [m overrides]
  (objectify (merge (m :map) overrides))
)
