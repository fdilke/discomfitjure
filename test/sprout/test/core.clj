(ns sprout.test.core
  (:use [sprout.core])
  (:use [clojure.test]))

(deftest test-ack ;; sample test, Ackermann function
	(is 1 (ack 0 0))
	(is 61 (ack 3 3))
)

(deftest test-puff
  (is (= '(()())  (puff-up '()))))

(deftest test-pure
  (is (= true (pure? '())))
  (is (= true (pure? '(()()))))
  (is (= false (pure? :hogwash)))
  (is (= false (pure? '(:hogwash))))
  (is (= true (pure? (nth (iterate #(list % %) '()) 7))))
)

(run-tests)