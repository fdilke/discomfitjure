(ns sprout.scratch
  (:use sprout.monoid sprout.endovec-monoid)
)

(defn explore 
  "explore the 2-generator submonoids of End(n)" 
  [n]
  (let [M (all-endos n)
        m-set (M :set)
     ] (doall (for [x m-set y m-set]
         (let [T (endovec-monoid x y)
               d2  (count (omega-autos T))
               d2a (count (omega-autos (opposite T)))
         ]  (println x y (T :size) "/" d2 d2a)
)) 
)))
    
