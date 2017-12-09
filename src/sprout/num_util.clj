(ns sprout.num-util)

(defn gcd [M N] (loop [m M n N] (cond (< m n) (recur n m)
                                      (= n 0) m
                                      :default (recur (- m n) n)
)))
