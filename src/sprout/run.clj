; startup script for 'lein run'

(ns sprout.run
  (:use clojure.set
     sprout.monoid sprout.endovec-monoid sprout.endovec
))

(println "Discomfitjure run...")

#_ [
(def monoids4 (filter (fn [M] (= 4 (count (omega M)))) 
              (submonoids (all-endos 3))))

(println "test each monoid = "
	(for [M monoids4] (let [
     M-set (M :set)                         
	   A (apply union (filter #(not= M-set %) (omega M)))
	   B (apply intersection (filter not-empty (omega M)))
	   ] (assert (< (count A) (count M-set)))
       (assert (< 0 (count B) (count A)))
	     (= B (set (for [x A y A] (M :mul x y))))
)))

(defn -main [& args]
  (println "# 4-monoids = " (count monoids4))
)

(def M (endovec-monoid [0 2] [1 0 0]))
]