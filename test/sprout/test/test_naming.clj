(ns sprout.test.test-naming
    (use clojure.test clojure.set sprout.naming)
)

(deftest test-names (let [
   X #{:a :b :c :d}
   name-map (make-names short-name X { :b "3" :d "x"})
 ] (is (= "3" (name-map :b) ))
   (is (= "x" (name-map :d) ))
   (is (= #{" a" " b"} #{(name-map :a) (name-map :c)}))
))


