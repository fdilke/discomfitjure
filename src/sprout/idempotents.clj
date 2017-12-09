(ns sprout.idempotents
  (:use clojure.contrib.combinatorics)
)

; (println "xxx =" (cartesian-product [1 2] [:x :y :z] [true false]))

(defn count-idempotents-0
  "Return the number of idempotent functions on a set with n elements"
  [n] (let [
    all-fns (apply cartesian-product (repeat n (range n)))
    idempotent? (fn [f] (= f (map (vec f) f)))
  ] (count (filter idempotent? all-fns))
))

(defn count-idempotents
  "Return the number of idempotent functions on a set with n elements"
  [n] (if (= n 0) 1
    (apply + (for [r (range 1 (inc n))]
       (* (count (combinations (range n) r)) (apply * (repeat (- n r) r)))
))))