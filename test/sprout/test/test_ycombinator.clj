(ns sprout.test.test-ycombinator
  (use clojure.test sprout.ycombinator)
)

(deftest test-recurse
  (testing "Recursive factorial"
     (let [fact (ycombinator (fn [f] (fn [n]
            (if (= n 0) 1 (* n (f (dec n))))
     )))] 
     (is (= 1 (fact 0)))
     (is (= 6 (fact 3)))
     (is (= 720 (fact 6)))
  )) (testing "Recursive Fibonacci"
     (let [fibo (ycombinator (fn [f] (fn [n] 
            (if (> n 1) (+ (f (- n 1)) (f (- n 2))) 1)
     )))] 
     (is (= [1 1 2 3 5 8 13] (map fibo (range 7))))
  )) (testing "Recursive Ackermann"
     (let [ack (ycombinator (fn [f] (fn [[m n]] (cond 
            (= m 0)   (+ n 1)
            (= n 0)   (f [(- m 1) 1])
            :default  (f [(- m 1) (f [m (- n 1)])])
     ))))] 
     (is (= [61] (map ack [[3 3]])))
)))

