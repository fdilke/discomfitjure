(ns sprout.d-i-y (:use clojure.set))

(defn my-comp 
  ([] identity)
  ([f & etc] (fn [x] (f ((apply my-comp etc) x))))
  )

(defn my-partial [f & args]
  (fn [& other-args]
    (apply f (concat args other-args)))
  )

