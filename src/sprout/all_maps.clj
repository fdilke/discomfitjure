(ns sprout.all-maps
)

(defn all-maps [src dest]
  (cond (empty? src) #{{}}
        (empty? dest) #{}    
		    true (let [[x & rest] (apply list src)
		         rest (apply list rest)]
		        '(prn "x, rest, dest = " [x rest dest])
		        '(prn "for clause =>" (for [m (all-maps rest dest) y dest]
		        [m y]))
		      (set (for [m (all-maps rest dest) y dest]
		        (assoc m x y)
)))))


