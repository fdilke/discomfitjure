(ns sprout.test.test-my-oo
  (use clojure.test sprout.my-oo)
)

(deftest my-oo-map-lookup
  (let [sample-map { 
    :the-int 5
    :the-string "hogwash"
    :non-key (fn ([_] 7) ([_ x] (+ 1 x))) 
    }
    oo-map (objectify sample-map)    
   ] (is (= 5 (oo-map :the-int)))
     (is (= "hogwash" (oo-map :the-string)))
     (is (= sample-map (oo-map :map)))
     (is (= 5 (oo-map :map :the-int)))
     (is (= 7 (oo-map)))
     (is (= 42 (oo-map 41)))
))

(deftest my-oo-passes-this
  (let [sample-map {
    :get-this (fn [this] this)
    :non-key (fn [this & rest] this)
   }
   oo-map (objectify sample-map)
   ]
   (is (= oo-map (oo-map)))
   (is (= oo-map (oo-map 0)))
   (is (= oo-map (oo-map :get-this)))
))

(deftest my-oo-functionizes
  (let [sample-map {
     :two 2
     :non-key "Hi"
     :the-fn (fn [_ x y] (+ x y))
     }
     oo-map (objectify sample-map)]
    (is (= "Hi" (oo-map :non-key)))
    (is (= 8 (oo-map :the-fn 3 5)))
))

(deftest test-inherit
  (let [oo-map (objectify {
     :string "Hello"
     :int 17
     :func (fn [this x] (inc x))
     :func2 (fn [this x] (- x 1))
     :indirect (fn [this x] (this :func3 x))
     }) child (inherit oo-map {
     :int 22
     :func2 (fn [this y] (* 2 y))
     :func3 (fn [this y] (* 7 y))
     })] 
     (is (= "Hello" (child :string)))
     (is (= 22 (child :int)))
     (is (= 7 (child :func 6)))
     (is (= 12 (child :func2 6)))
     (is (= 14 (child :func3 2)))
     (is (= 14 (child :indirect 2)))
))


