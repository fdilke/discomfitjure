(ns sprout.endovec
  (:use sprout.util)
)

; Functions for working on endomorphism vectors, which are vectors expressing an endomorphism of a finite ordinal:
; e.g. [2 3] sends 0->2, 1->3, presumably on some n for n>=4

(defn mul-endovec
  "Multiplication of endomorphism vectors"
  [x y] 
  (vec (map y x))
)

(defn extend-endovec
  "Pad an endovec up to a required length with identity mappings"
  [vec length]
  (concat vec (range (count vec) length))
)

(defn normalize-endovecs
  "Make a set of endovecs all have the same length and include the identity"
  [g]
  (let [max-implied (apply max 0 (apply concat (for [x g] (map inc x))))
        n (apply max max-implied (map count g))
        id (range 0 n)
  ] (map vec (cons id (map #(extend-endovec % n) g))))
)

(defn all-endos-set
  "List all endomorphisms on an integer n, as vectors"
  [n] (tuples n (range 0 n))
)