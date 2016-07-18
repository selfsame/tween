(ns tween.test 
  (:use 
    tween.core
    arcadia.core
    arcadia.linear))

(defn t1 []
	(let [o (create-primitive :cube)]
    (timeline* :loop
      (tween {:local {:scale (v3 2.0)}} o 0.5)
      (wait 0.2)
      (tween {:local {:scale (v3 1.0)}} o 0.5)
      (tween 
        {:local {:position (v3 (rand-nth [-1 1]) 0 (rand-nth [-1 1]))}} 
          o 1.0))))

(comment     
  (timeline-1 (cycle [
    (fn [] (wait 0.25))
    (fn [] (tween 
      {:local {:position (v3 (rand-nth [-1 1]) 0 (rand-nth [-1 1]))}} 
        o 1.0))])))