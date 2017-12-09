(ns sprout.test.test-all-maps
  (:use sprout.all-maps clojure.test)
)

(deftest test-all-maps
  (is #{{}} (all-maps '() '()))
  (is #{{}} (all-maps '() '(1 2)))
  (is #{{}} (all-maps [] []))
  (is #{}   (all-maps '(:a) '())) 
  (is #{{:a :b}} (all-maps [:a] [:b]))
  (is #{{:a :b}} (all-maps #{:a} #{:b}))
)

(run-tests)