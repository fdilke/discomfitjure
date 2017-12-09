(ns sprout.test.test-spike
  (use clojure.test sprout.spike)
)

(deftest test-self-returning-func
  (is (= self-returning-func (self-returning-func)))
)

