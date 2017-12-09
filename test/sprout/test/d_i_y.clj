(ns sprout.test.d-i-y 
  (:use clojure.test)
  (:use [sprout.d-i-y]))

(deftest test-my-comp
  (is (= 9 ((my-comp inc inc) 7)))
  (is (= 11 ((my-comp inc #(* 2 %)) 5)))
)

(deftest test-my-partial
  (is (= 10 ((my-partial + 1 2) 3 4)))
  )

(run-tests)