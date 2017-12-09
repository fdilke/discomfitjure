(ns sprout.test.test-num-util
  (:use clojure.test
        sprout.num-util)
)

(deftest test-gcd
  (is (= 1 (gcd 2 3)))
  (is (= 1 (gcd 9 8)))
  (is (= 2 (gcd 2 4)))
  (is (= 6 (gcd 12 30)))
)
