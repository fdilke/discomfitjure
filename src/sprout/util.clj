(ns sprout.util
  (:use clojure.set)
)

(defn power-set
  "Calculate the power-set of a collection"
  [coll]
  (if (empty? coll) #{#{}}
    (let [x (first coll)
          more (rest coll)
          power-more (power-set more)]
      (set (union power-more (for [Y power-more] (into Y [x]))))
)))

(defn tuples
  "List all n-tuples from a set"
  [n coll] (set (reduce (fn [vecs coll]
                (apply union (map (fn [v] (for [x coll] (conj v x))) vecs))
                ) [[]]
                (replicate n coll))
))

(defn tuple-sets
  "List all tuples of n elements from a collection, each with its elements in order of appearance"
  [n coll]
  (let [size (count coll) 
     ] (cond
          (= n 0)             #{[]}
          (> n size)          #{}
          (= n size)          #{(vec coll)}
          :default
          (let [[x & more] coll]
            (set (union (map #(into [x] %) (tuple-sets (dec n) more))
                   (tuple-sets n more)))
)))) 

(defn involutions 
  "Return a set of all involutive maps x=>x"
  [coll] 
  (if (empty? coll) #{{}}
    (let [x (first coll)
          other (rest coll)]
      (set (union (for [Y (involutions other)] (assoc Y x x))  ; leaving x fixed
        (apply union (for [z other]  
                         (for [Y (involutions (remove #(= z %) other))] 
                         (assoc Y x z z x)
))))))))

(defn map-classes
  "Given a collection X and a function f on it, return a map sending each
  element of X to the corresponding equivalence class, where x ~ y iff f(x)=f(y)"
  [X f] (let [
    f-map (zipmap X (map f X))
    values (vals f-map)
  ] (apply merge (for [V values] (let [
    eq-class (set (filter #(= V (f-map %)) X))
  ] (apply hash-map (interleave eq-class (repeat (count eq-class) eq-class)))
  )))
))

