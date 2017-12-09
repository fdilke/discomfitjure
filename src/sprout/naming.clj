(ns sprout.naming
  (use clojure.set)
)

(defn short-name [n] (letfn [(
   to-char [i] (char (+ (int \a) i))
  )] (if (< n 26) 
     (str " " (to-char n))
     (str (to-char (dec (/ n 26))) (to-char (mod n 26)))
)))

(defn make-names
  "Make up names for the objects in X, using the given name
  generator, and given a map of fixed ones"
  [name-gen X start-map] (let [
    unassigned-X (apply disj (set X) (keys start-map))
    new-names (map name-gen (range (count unassigned-X)))
  ] (merge start-map (zipmap unassigned-X new-names))
))                           
