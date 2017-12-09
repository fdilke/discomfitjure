(ns sprout.monoid
  (:use clojure.set sprout.util sprout.my-oo sprout.all-maps sprout.naming
        clojure.contrib.combinatorics
        [clojure.string :only (upper-case join) :rename {join string-join}]
))

; T=G, X=G the set of generators
; 
; Loop: X = XG � T
; If X empty, stop, answer is T
; T += X

(defn generate-monoid-less-setty
  "Generate a monoid from the specified elements"
  [mul g]
  (letfn [(mul-sets [X Y]
           (for [x X y Y] (mul x y))
   )] (loop [t (set g) 
             x g
    ] (let [xg (mul-sets x g)
           xg_t (remove t xg)]
           (if (empty? xg_t) t (recur (into t xg_t) xg_t)))
)))

(defn generate-monoid
  "Generate a monoid from the specified elements"
  [mul g]
  (letfn [(mul-sets [X Y]
           (set (for [x X y Y] (mul x y)))
   )] (loop [t (set g) 
             x t
    ] (let [xg (mul-sets x g)
           xg_t (difference xg t)]
           (if (empty? xg_t) t (recur (union t xg_t) xg_t)))
)))

(defn monoid
  "Create a monoid object with the given set, identity and multiplication"
  [m-set id mul]
     (objectify {
       :set m-set
       :id id
       :mul (fn [this x y] (mul x y))
       :size (count m-set)
       :generate (fn [this gens] (generate-monoid (partial this :mul) (conj gens (this :id))))
}))

(defn submonoid [M subset]
    (monoid subset (M :id) (partial M :mul))
)

(defn opposite
  "Construct the opposite of a monoid, by reversing multiplication"
  [M]
  (letfn [(opp-mul [x y] (M :mul y x))
  ] (monoid (M :set) (M :id) opp-mul) 
))

(defn num-gens
    "Find the minimum number of generators of a monoid"
  [M]
  (let [m-set (M :set)
        size (M :size)]
  (letfn [(can-gen
            [subset n] (if
               (= n 0) (= size (count subset))   
               (some #(can-gen (M :generate (cons % subset)) (dec n)) 
                     (difference m-set subset))))]
         (loop [n 0] (if
             (can-gen #{(M :id)} n)   n
             (recur (inc n))
)))))

(defn omega
  "Calculate the subobject classifier of the topos of M-sets, M a monoid"
  [M]
  (let [m-set (M :set)
       cyclics (set (for [x m-set] (set (for [y m-set] (M :mul x y)))))
       ] (set (for [X (power-set cyclics)] (apply union X))
)))

(defn omega-mul
  "Right multiplication on omega: [V]x = { y: xy <- V }"
  [M V x]
  (set (filter (fn [y] (V (M :mul x y))) (M :set)))
)

; experimental - automorphisms of omega
; X^Y = Hom(MxY, X)
; Hom-set(MxV, O) identified with Hom-set(M, O^O)
; and we know the image of each m in M is an involutive set-map of O
; so: select all f:M -> involutions on O with:
; the map h: (m, V) -> f(m)(V) satisfying:
;         h(m, V)n = h(mn, V\n)
; i.e.    f(m)(V)\n = f(mn)(V\n)
; for all m, n in M and V in O

(defn omega-autos 
  ([M] (omega-autos M (omega M)))
  ([M O] 
    (let [O (omega M)
        m-set (M :set)
        invs (involutions O)
        maps (all-maps m-set invs)
        is-auto? (fn [f]
          (every? (fn [m]
          (every? (fn [n]
          (every? (fn [V] (=
             (omega-mul M ((f m) V) n)
             ((f (M :mul m n)) (omega-mul M V n))
          ))   O 
          ))   m-set
          ))   m-set
     ))] (filter is-auto? maps)
)))

(defn- tabulate
  [symbol rows cols d-row d-col d-entry] (str
  "\n"                                    
  (apply str " " symbol "|"
    (for [c cols] (d-col c))
  ) "\n"
  (apply str "--+"
    (for [c cols] "--")
  ) "\n"
  (apply str (for [r rows] (str
    (d-row r)
    "|"
    (apply str (for [c cols]
       (d-entry r c)
    ))
    "\n"
  ))
)))

; also experimental. Given a monoid, draw up a little table
; showing the action of the nontrivial automorphism of omega

(defn show-auto [M] (let [
   O         (omega M)
   O-autos   (omega-autos M O)
   trivial-involution? (fn [i]  
      (every? #(= % (i %)) (keys i)))
   nontrivial-auto? (fn [K] (not (every? (fn [i] 
      (trivial-involution? i)) (vals K))))
   nontrivs  (filter nontrivial-auto? O-autos)
   _ (assert (= 1 (count nontrivs)))
   the-auto (nth nontrivs 0)
   m-id (M :id)
   m-name-dict (make-names short-name (M :set) { m-id " 1"})
   o-name-dict (make-names (comp upper-case short-name) O { #{} " 0" (M :set) "[]"})
   m-list (cons m-id (apply list (difference (M :set) #{m-id})))
   o-list (concat [#{}] (apply list (difference O #{#{} (M :set)})) [(M :set)])
   m-show-product (fn [m n] (m-name-dict (M :mul m n)))
   show-omega-product (fn [V m] (o-name-dict (omega-mul M V m)))
   show-omega-exp (fn [V m] (o-name-dict ((the-auto m) V)))
   ] (apply println (concat
       (for [m m-list]
           (str (m-name-dict m) " = " m)
       ) [(tabulate \* m-list m-list m-name-dict m-name-dict m-show-product)]
       (for [V o-list] (let [name (o-name-dict V)]
          (str " " name " = {" 
               (apply str (map m-name-dict (filter V m-list))) 
               "}\n")
        )) [(tabulate \: o-list m-list o-name-dict m-name-dict show-omega-product)
          (tabulate \^ o-list m-list o-name-dict m-name-dict show-omega-exp)
         ]
))))

; also experimental. 

(defn calc-H
  "Calculate the H-classes of a monoid"
  [M] (let [
    Mx (fn [x] (set (map #(M :mul % x) (M :set))))            
    xM (fn [x] (set (map #(M :mul x %) (M :set))))            
    f-H (fn [x] [(Mx x) (xM x)])            
  ] (map-classes (M :set) f-H)
))

(defn show-green
  "Show an analysis of the Green H-classes of a monoid"
  [M] (let [
   H-map (calc-H M)            
   m-id (M :id)
   m-list (cons m-id (apply list (difference (M :set) #{m-id})))
   m-name-dict (make-names short-name (M :set) { m-id " 1"})
   m-show-product (fn [m n] (m-name-dict (M :mul m n)))
  ] (apply println (concat 
    [(tabulate \* m-list m-list m-name-dict m-name-dict m-show-product)]
    (for [C (set (vals H-map))] (apply str (concat ["{"] (map m-name-dict C) [" }"])))
))))

(defn goop?
  "Test if a monoid is a 'goop', i.e. has a unique nontrivial right ideal"
  [M]
  (= 3 (count (omega M)))
)

(defn group?
  "Test a monoid for group-hood"
  [M]
  (let [m-set (M :set)
        id (M :id)
        are-inverse (fn [x y] (= (M :mul x y) id))
        has-right-inverse? (fn [x] (some (partial are-inverse x) m-set)) 
  ] (every? has-right-inverse? m-set)
))

(defn subgrp?
  "Tell if a subset of a monoid is a 'subgrp', i.e. contains an
  idempotent e and is a group with e as unit"
  [M S] (let [
   idempotent? (fn [e] (= e (M :mul e e)))
   idempotents (filter idempotent? S)
   ] (and (= 1 (count idempotents)) (let [
     e (first idempotents)                                          
     SS (set (for [x S y S] (M :mul x y)))
   ] (and 
       (subset? SS S)
       (every? (fn [x] (= x (M :mul e x) (M :mul x e))) S)
       (group? (monoid S e #(M :mul %1 %2)))
)))))

(defn factor-monoid 
  "Calculate the quotient of a monoid by a congruence expressed as a function"
  [M cong] (let [
    cong-map (map-classes (M :set) cong)                 
    F (set (vals cong-map))
    F-id (cong-map (M :id))
    F-mul (fn [X Y] (cong-map (M :mul (first X) (first Y))))
    ] (monoid F F-id F-mul)
))

(defn verify-green
  "Verify the conclusions of Green's theorem for the H-classes of a monoid"
  [M] (let [
   H-classes (set (vals (calc-H M)))
   idempotent? (fn [e] (= e (M :mul e e)))
   Tx (memoize (fn [x] (set (filter (fn [y]
         (some (fn [z] (= (M :mul z x) (M :mul x y))) (M :set))                             
         ) (M :set)))))
   Ix (memoize (fn [x] (set (filter (fn [y]
         (some (fn [Y] 
           (= x (M :mul x (M :mul y Y)) (M :mul x (M :mul Y y))) 
           ) (Tx x))
         ) (Tx x)))))
  ] (for [X H-classes] (let [
      x (first X)                             
	    cong (fn [y] (M :mul x y))
      Gx (factor-monoid (submonoid M (Ix x)) cong)
     ]
      (doseq [y X] (assert (= (Tx x) (Tx y))) 
                   (assert (= (Ix x) (Ix y))))
      (assert (group? Gx))
      (assert (= (Gx :size) (count X)))
      (if (some idempotent? X)
          (assert (subgrp? M X)) (let [
            XX (set (for [x X y X] (M :mul x y)))
          ] (assert (empty? (intersection X XX)))
))))))

; previous slow-and-solid algorithm, for comparison

(defn submonoids-slow [M]
  (let [m-set (M :set)
        cyclics (set (for [x m-set] (M :generate #{x})))
        sets-cyclics (power-set cyclics)
        unions (set (for [K sets-cyclics] (conj (apply union K) (M :id))))
        sub-m-sets (set (for [u unions] (M :generate u)))
     ]  
    (map (partial submonoid M) sub-m-sets)
))

(defn submonoids
  "Enumerate the submonoids of a monoid"
  [M]
  (letfn [(all-above-using-without [S G E]
          (if (empty? G) [S] (let [[x & rest] G]
          (if (some #{x} S) (recur S rest E)
          (if (some #{x} E) (recur S rest E)
            (let [Sx (M :generate (conj S x))
                  S-plus (all-above-using-without S rest (conj E x))
          ] (if (some E Sx) S-plus
              (into S-plus (all-above-using-without Sx rest E)
       ))))))))            
  ] (let [m-list (apply list (M :set))
          subsets (all-above-using-without [(M :id)] m-list #{})
 ] (map (comp (partial submonoid M) set) subsets) 
)))

(defn cyclic-group
  "Construct the cyclic group of order n"
  [n]
  (let [m-set (set (range n))
        id 0
        mul (fn [x y] (mod (+ x y) n))
        ] (monoid m-set id mul)
))

(defn compose-partial-maps 
  [f g] (let [
    k (apply list (keys f))
    gf (comp g f)
    with-nils (zipmap k (map gf k))
    ] (select-keys with-nils (filter #(not (nil? (gf %))) k))
))

(defn partial-bijs
  "The monoid of partial bijectives on a set of n elements"
  [n] (let [
  X (range n)            
  maps (set (apply concat 
     (for [r (range (inc n))]  
       (let [Cs (combinations X r)] 
         (apply concat (for [C Cs D Cs] 
           (let [Clist (apply list C)] 
             (for [E (permutations D)] 
               (apply hash-map (interleave C E))))))))))
  id (zipmap X X)
  ] (monoid maps id compose-partial-maps))
)
