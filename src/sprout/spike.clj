(ns sprout.spike
  (:use clojure.set 
    sprout.endovec-monoid sprout.monoid)
)

; a function that returns itself 

(def self-returning-func-old 
  (let [selfref (ref nil)
        the-func #(deref selfref)]
     (dosync (ref-set selfref the-func))
     the-func
))

(def self-returning-func 
  (letfn [(the-func [] the-func)]
     the-func
))

(defn sq-in-sq 
  "Calculate the number of squares in an n-by-n square"
  [n]
  (apply + (map #(* % %) (range (inc n))))
)

'(def goops3 (filter goop? (submonoids (all-endos 3))))
'(defn analyze [G] (print "Hmmm... a goop:" (G :size) (num-gens G)) (println " -- " (count (omega-autos G))))
'(map analyze goops3)
'(def monoids24 (let [M (all-endos 4) m (M :set)] (for [x m y m] (endovec-monoid x y))))
'(def goops24 (filter goop? monoids24))

'(def qq (proxy [java.lang.Runnable] [] (run [] (println "wey hey hey"))))

'(def xx (proxy [clojure.lang.AFn] [] (invoke [x] (println "object is:" x))))
(def i-am-two (proxy [clojure.lang.AFn] [] 
                (invoke [x] (println "object is:" x))
                (equals [x] (= x 2))
))
