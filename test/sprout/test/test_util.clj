(ns sprout.test.test-util
  (:use clojure.test sprout.util)
)

(deftest test-power-set
  (is (= #{#{}} (power-set [])))
  (is (= #{#{} #{:x}} (power-set [:x])))
  (is (= #{#{} #{3} #{8} #{3 8}} (power-set [3 8])))
  (is (= #{#{} #{3} #{8} #{7} #{3 8} #{8 7} #{3 7} #{3 8 7}} (power-set [3 7 8])))
  (is (= 32 (count (power-set (range 5)))))
)

(deftest test-tuples
  (is (= #{[]} (tuples 0 [6 7]))) 
  (is (= #{[6] [7]} (tuples 1 [6 7]))) 
  (is (= #{[6 7] [7 8] [6 8] [7 6] [8 7] [8 6] [6 6] [7 7] [8 8]} (tuples 2 [6 7 8]))) 
)

(deftest test-tuple-sets
  (is (= #{[]} (tuple-sets 0 [6 7]))) 
  (is (= #{[6] [7]} (tuple-sets 1 [6 7]))) 
  (is (= #{[6 7] [7 8] [6 8]} (tuple-sets 2 [6 7 8]))) 
  (is (= #{[6 7 8]} (tuple-sets 3 [6 7 8]))) 
)

(deftest test-involutions
  (is (= #{{}} (involutions [])))
  (is (= #{{:x :x}} (involutions [:x])))
  (is (= #{{1 2 2 1}{1 1 2 2}} (involutions [1 2])))
  (is (= #{{1 1,2 2,3,3} {1 2,2 1,3 3} {1 1,2 3,3 2} {1 3,2 2,3 1}} (involutions [1 2 3])))
)

(deftest test-map-classes (let [
  X (range 5)
  f #(mod % 2)
  evens #{0 2 4}
  odds #{1 3}
  expected { 0 evens 2 evens 4 evens 1 odds 3 odds }
  ] (is (= expected (map-classes X f)))
))



