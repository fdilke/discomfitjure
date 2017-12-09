(ns sprout.kazunk.clique
  (:use clojure.set)
)

(declare max-cliques)

(defn max-cliques-graph
  "List the maximal cliques in a graph"
  [neighbours G]
  (loop [nodes G
  ] (if (empty? nodes) #{#{}}
  (let [[x & rest] nodes 
        hh (max-cliques neighbours x)
  ] hh
))))

'(defn max-cliques-node
  "List the maximal cliques of a node in a graph"
  [neighbours x]
  (letfn [(max-c-above [C]
     (let [candidates (apply intersection (map neighbours C))
      ] (if (empty? candidates) #{C}
        #{#{:x :y}} ; fix properly later
  )))] (max-c-above #{x})
))

(defn max-cliques
  "List the maximal cliques of a node in a graph"
  [neighbours x]
  (letfn [(bron-kerbosch1 [R P X]
     (if (and (empty? P) (empty? X)) [R]
         (loop [p P
                x X
                acc []
         ](if (empty? p) acc
             (let [v (first p)
                   N (set (neighbours v))
                   restrict #(intersection N (set %))
                   cliques (bron-kerbosch1 (conj R v) (restrict p) (restrict x) )
             ] (recur (rest p) (conj x v) (into acc cliques))
  )))))]
  (set ( bron-kerbosch1 #{x} (neighbours x) []))
))