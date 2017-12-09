(ns sprout.core)

(defn ack [m n] (cond
	(= m 0) (+ n 1)
	(= n 0) (ack (- m 1) 1)
	true (ack (- m 1) (ack m (- n 1)))
))
		

(defn average [x y] (/ (+ x y) 2))

(defn puff-up [x] (list x x))

(defn pure? [x] (and (list? x) (every? identity (map pure? x))))


