(ns tween.test
  (:use 
    tween.core
    pdfn.core
    arcadia.core
    hard.core
    hard.input))



(comment 

'(defn start-demo [o] 
  (clear-cloned!)
  (mapv destroy! (every ball))
  (log (count (every ball)))
  (hook+ (clone! :ground) :update #'tween.test/-update )
  (hook+ (the ground-plane) :on-mouse-over #'tween.test/click))
 


'(defn move [^UnityEngine.GameObject o]
  (timeline [
    (wait (rand))
    (tween {:local {:scale (->v3 (rand) (rand) (rand))}
            :position (V+ (->v3 o) 
      (->v3 0 (- (rand 10) 5) 0))} o 
    10.0)
    #(do (move o) nil)]))

'(do (clear-cloned!)
  (dorun (for [x (range 15) z (range 15) 
    :let [o (clone! :ball (->v3 x 0 z))]]
    (move o ))))







'(defn environ [spaces]
  (with-out-str (pprint/print-table 
    (vec (flatten (map 
      (comp 

        (fn [[nss s]] (map (partial hash-map s) nss)) 
        (juxt (comp #(map first %) ns-publics) str)) 
      spaces))))))


'(defn report [-ns] 
  (set! (.* (the repl) >Text.text) 
    (apply str (interpose "\n" 
      (cons (str (find-ns -ns) "\n----------")
        (map first (ns-publics (find-ns -ns))))))) true)




(defn pulse [^UnityEngine.GameObject o c speed]
  (timeline [   
    (tween {:material {:color c}} o (* speed 0.5))
    (wait speed)
    (tween {:material {:color (color 0 1 1)}} o (* speed 0.5))
    #(do (pulse o c speed) nil)]))

(defn curl [^UnityEngine.GameObject o delay ]
  (timeline [
    (tween {:local {:scale (->v3 0.97)}} o 0.01)
    (tween {:local {:euler (->v3 1  0 1)}}   o 2.5)
    (tween {:local {:euler (->v3 -4 1 14)}}   o 2.5)
    #(do (curl o delay) nil)]))

(do 
  (clear-cloned!)
(destroy! (every Cube))

(dorun (for [x (range 10) z (range 1)]
  (reduce #(local-position! (parent! (clone! %1) %1) (->v3 0 (do %2 1)  0))  
  (position! (parent!  (create-primitive :cube) 
    (rotate! (clone! :ball) (->v3 0 (* x 40) 0))) 
    (->v3 (* x 0) 0 (* z 0))) 
  (range 40)))) 

 (mapv (juxt curl 
  #_#(pulse %1 (color 1 0 0) (* %2 0.01)) ) 
   (map ->go (every Cube)) (map #(* % 0.05) (range 10000)))
 true))

