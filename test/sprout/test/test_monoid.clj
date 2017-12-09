(ns sprout.test.test-monoid
  (use clojure.test sprout.monoid sprout.endovec-monoid
))

(deftest test-cyclic-group
  (let [M (cyclic-group 7)
   ] (is (= 7 (M :size)))
     (is (= 0 (M :mul 5 2)))
     (is (= 0 (M :id)))
))

; generate a monoid given multiplication and a set of elements (which had better include the identity)
(deftest test-generate-monoid
  (let [M (all-endos 3)
        mul (partial M :mul)
  ] (is (= #{[0 1 2][1 2 0][2 0 1]} (generate-monoid mul #{[1 2 0]})))
    (is (= #{[0 1 2][0 0 1][0 0 0]} (generate-monoid mul #{[0 1 2][0 0 1]})))
))

; the internal 'generate' method on monoids

(deftest test-generate-monoid
  (let [M (all-endos 3)
        C (cyclic-group 9)
  ] (is (= #{[0 1 2][1 2 0][2 0 1]} (M :generate #{[1 2 0]})))
    (is (= #{[0 1 2][0 0 1][0 0 0]} (M :generate #{[0 1 2][0 0 1]})))
    (is (= #{[0 1 2][0 0 1][0 0 0]} (M :generate #{[0 0 1]}))) ; don't need the 1
    (is (= #{0 3 6} (C :generate #{3})))
    (is (= (C :set) (C :generate #{4})))
))

; wrapping up a subset as a submonoid
(deftest test-submonoid 
  (let [M (all-endos 2)
        s #{[0 1][0 0]}
        S (submonoid M s)
       ]
   (is (= s (S :set)))
   (is (= [0 0] (S :mul [0 0] [0 1])))
))

(deftest test-num-gens
  (is (= 0 (num-gens (symmetric-group 0))))
  (is (= 0 (num-gens (symmetric-group 1))))
  (is (= 1 (num-gens (symmetric-group 2))))
  (is (= 2 (num-gens (endovec-monoid [1 0] [1 2 0]))))
  (is (= 3 (num-gens (endovec-monoid [1 0] [0 1 3 2] [0 1 2 3 5 4]))))
  (is (= 2 (num-gens (all-endos 2))))
)

(deftest test-omega
  (is (= #{#{} #{[]}} (omega (symmetric-group 0))))
  (is (= #{#{} #{[1 1]} #{[0 0]} #{[1 1] [0 0]} #{[0 1] [1 0] [0 0] [1 1]}}
         (omega (opposite (all-endos 2)))))
)

(deftest test-omega-mul
  (let [M (opposite (all-endos 2))
        V #{[0 0]}
       ] 
    (is (= V (omega-mul M V [0 1])))
    (is (= #{[1 1]}(omega-mul M V [1 0])))
))

(deftest test-goop?
  (is (not (goop? (endovec-monoid))))
  (is (not (goop? (symmetric-group 2))))
  (is (goop? (all-endos 2)))
  (is (not (goop? (opposite (all-endos 2)))))
)

(deftest test-opposite
  (let [M (symmetric-group 3)
        O (opposite M)
        x [0 2 1]
        y [1 0 2]
        z [2 0 1]
       ]
     (is (= z (M :mul y x)))
     (is (not= z (M :mul x y)))
     (is (= z (O :mul x y)))
     (is (= (M :id) (O :id)))
     (is (= (M :set) (O :set)))
))

(deftest test-omega-autos
  (is (= 2 (count (omega-autos (symmetric-group 0)))))
)

; make sure the old and new submonoid-enumerators coincide
(deftest test-submonoids-compare-new-and-old
  (doall (for [M [(all-endos 2) (symmetric-group 3) (cyclic-group 9)]
         ] (let [sub1 (submonoids-slow M)
                 sub2 (submonoids M)
                 to-sets (comp set (partial map #(% :set)))
         ] (is (= (to-sets sub1) (to-sets sub2)))
))))

(deftest test-submonoids
  (let [M (all-endos 2)
        subM (submonoids M)
        subMset (set (for [S subM] (S :set)))
    ] (is (= 6 (count subMset)))
    (is (= #{#{[0 1]} #{[0 1][1 0]} #{[0 1][0 0]} #{[0 1] [1 1]} #{[0 1][0 0][1 1]} #{[0 1][0 0][1 1][1 0]}}
           subMset))
    (is (= 4 (count (submonoids (cyclic-group 6)))))
))

(deftest test-group?
  (is (= true (group? (symmetric-group 3))))
  (is (= true (group? (cyclic-group 6))))
  (is (= false (group? (all-endos 2))))
)

(deftest test-factor-monoid (let [
   C6 (cyclic-group 6)
   cong2 (fn [x] (C6 :mul x x))
   F (factor-monoid C6 cong2)
   ] (is (= true (group? F)))
     (is (= 3 (F :size)))
))

(deftest test-compose-partial-maps
  (is (= {:a true :c false} (compose-partial-maps {:a 2 :b 3 :c 0} {2 true 0 false})))
)

(deftest test-partial-bijs
  (is (= [1 2 7 34 209] (map #((partial-bijs %) :size) (range 5))))
  (is (= #{{} {1 1} {1 0} {0 0} {0 1} {0 0 1 1} {0 1 1 0}} ((partial-bijs 2) :set)))
)

