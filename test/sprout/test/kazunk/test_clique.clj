(ns sprout.test.kazunk.test-clique
  (:use clojure.test clojure.set
        sprout.num-util
        sprout.kazunk.clique)
)

(deftest test-max-cliques
  (is (= #{#{:x}} (max-cliques { :x [] } :x)))
  (is (= #{#{:x :y}} (max-cliques { :x [:y] :y [:x] } :x)))
)

(deftest test-max-cliques-2
  (let [is-neighbour? (fn [x y] (odd? (+ x y)))
        neighbours (fn [x] (filter (partial is-neighbour? x) (range 7)))
       ] (is (= #{#{0 1}#{0 3}#{0 5}} (max-cliques neighbours 0)))
))

(deftest test-max-cliques-3
  (let [is-neighbour? (fn [x y] (and (not= x y) (< 1 (gcd x y))))
        neighbours (fn [x] (filter (partial is-neighbour? x) (range 1 10)))
       ] (is (= #{#{2 4 6 8}} (max-cliques neighbours 2)))
))

(deftest test-max-cliques-graph
  (is (= #{#{}} (max-cliques-graph {} [])))
  (is (= #{#{:x}} (max-cliques-graph {:x []} [:x])))
)
       