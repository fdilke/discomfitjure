(ns sprout.sphere-problem)

(defn solve-sphere-problem
  "Estimate the average distance between 2 points in a unit sphere"
  [n]
  (let [
    sqr (fn [x] (* x x))
    recip-n-1   (/ 1.0 (dec n))
    the-range (range n)
    coords (for [i the-range] (+ -1 (* 2 i recip-n-1)))
    sphere-triples (for [i coords j coords k coords
                        :when (> 1 (+ (sqr i) (sqr j) (sqr k)))
                       ] [i j k])
    num-triples (count sphere-triples)
    distance (fn [[i1 j1 k1] [i2 j2 k2]] (Math/sqrt 
       (+ (sqr (- i1 i2)) (sqr (- j1 j2)) (sqr (- k1 k2)))))  
    ] (/ 
        (apply + (for [x sphere-triples y sphere-triples] (distance x y)))
        1.0 (sqr num-triples))
))    

