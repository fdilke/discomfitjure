(ns sprout.test.test-endovec
  (use clojure.test sprout.endovec)
)

; Test multiplication of endomorphism vectors

(deftest test-mul-endovec
  (are [x y z] (= x (mul-endovec y z))
  [] [] []
  [1 0] [0 1] [1 0]
  [0 1 0] [0 2 0] [0 1 1]
))

(deftest test-extend-endovec
  (is (= [3 2 0 3 4 5] (extend-endovec [3 2 0] 6)))
)  

(deftest test-normalize-generators
    (are [x y] (= x (set (normalize-endovecs y)))
  #{[]}  #{}
  #{[0]} #{[0]}
  #{[1 1] [0 1]} #{[1]}
  #{[1 1 2][0 2 2][0 1 2]} #{[1] [0 2]}
))

(deftest test-all-endos-set
  (are [n x] (= (all-endos-set n) x)
  0 #{[]}
  1 #{[0]}
  2 #{[0 1] [0 0] [1 1] [1 0]}
) (is (let [e3 (all-endos-set 3)
            base (range 0 3)
            goodvector (fn [v] (and (= 3 (count v)) (every? #(some #{%} base) v))) 
    ] (and (= 27 (count e3))
          (every? goodvector e3))
)))

