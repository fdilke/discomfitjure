(ns sprout.test.test-endovec-monoid
  (use clojure.test sprout.endovec sprout.endovec-monoid)
)

(deftest test-monoid-id
  (is (= [] (monoid-id (all-endos-set 0))))
  (is (= [0 1] (monoid-id (all-endos-set 2))))
  (is (= [0 1 2] (monoid-id ((symmetric-group 3) :set))))
)

(deftest test-endovec-monoid
  (letfn [(monoid-set [& rest]
      ((apply endovec-monoid rest) :set)
   )]
  (is (= #{[]} (monoid-set)))
  (is (= #{[0 1] [1 0]} (monoid-set [1 0])))
  (is (= #{[0 1] [1 1]} (monoid-set [1])))
  (is (= #{[0 1] [0 0] [1 1]} (monoid-set [0 0] [1 1]))) 
  (is (= #{[0 1] [0 0] [1 1]} (monoid-set [0 0] [1]))) 
  (is (= #{[0 1 2] [0 2 1] [2 1 0] [1 0 2] [1 2 0] [2 0 1]} (monoid-set [1 0] [1 2 0]))) 
))

(deftest test-symmetric-group
 (let [sgs #((symmetric-group %) :set)]
  (is (= #{[]} (sgs 0)))
  (is (= #{[0]} (sgs 1)))
  (is (= #{[0 1] [1 0]} (sgs 2)))
  (is (= #{[0 1 2] [1 0 2] [2 1 0] [0 2 1] [1 2 0] [2 0 1]} (sgs 3)))
  (is (= 24 (count (sgs 4))))
  (is (= 120 (count (sgs 5))))
))

(deftest test-symmetric-group-multiplication
  (let [s3 (symmetric-group 3)
        s3_id (s3 :id)
        s3_mul (fn [x y] (s3 :mul x y))]
  (is (every? (fn [x] (= x (s3 :mul x s3_id))) (s3 :set)))
  (is (every? (fn [x] (= x (s3 :mul s3_id x))) (s3 :set)))
  (is [2 1 0] (s3_mul [0 2 1] [1 2 0]))
))
