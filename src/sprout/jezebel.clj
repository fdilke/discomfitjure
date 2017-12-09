(ns sprout.jezebel)

(defn average [& values] 
  (/ (apply + values) (count values))
)

(defn commonality
  [a b]
  (apply + (map * a b))
)

(defn mate 
  [parent1 parent2]
  (map average parent1 parent2)
)

(def greatgrandpa [1 0 0 0 0])
(def greatgrandma [0 1 0 0 0])
(def alice        [0 0 1 0 0])
(def sheila       [0 0 0 1 0])
(def andy         [0 0 0 0 1])
(def christopher (mate greatgrandpa greatgrandma))
(def john (mate greatgrandpa greatgrandma))
(def caroline (mate christopher alice))
(def tim (mate john sheila))
(def felix (mate caroline tim))
(def rosie (mate caroline tim))
(def jezebel (mate andy rosie))
(println "commonality =" (commonality felix jezebel))

  