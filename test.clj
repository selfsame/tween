(ns tween.test
  (:require [clojure.walk :as walk]
    [clojure.pprint :as pprint]
    [pdfn.core :refer [ppexpand]])
  (:use 
    arcadia.core
    hard.core
    hard.input
    tween.core))


(declare rand-action start-demo)

(defn wander [o] 
  (timeline [
  (wait (rand))
  ((tween {:position (V+ (->v3 o) (->v3 (rand-vec [-2 2] [0 0] [-2 2])))}) o 2)
  (rand-action o)]))

(defn throb [o] (timeline  [
  (wait (rand 2.0))
  ((tween {
    :local-scale (->v3 0.8 1.5 0.8)
    :material {:color (color (rand) (rand) 0)}
    :position (V+ (->v3 o) (->v3 0 2.5 0))} {:out :pow2}
    ) o 0.5)
  
  ((tween {
    :local-scale (->v3 1.5 0.4 1.5)
    :material {:color (UnityEngine.Color. 0.5 0.5 0.5)}
    :position (V+ (->v3 o) (->v3 0 -0.2 0))} {:in :pow2}
    ) o 0.5)
  ((tween {
    :position (V+ (->v3 o) (->v3 0 0.2 0))
    :local-scale (->v3 1)} {:out :pow4}
    ) o 0.8)
  (rand-action o)]))

(defn rand-action [^UnityEngine.GameObject o] #(do ((rand-nth [throb wander wander]) o) nil))

(defn click [o] 
    (let [point (V+ (first (map #(.point %) (ray-hits (mouse-ray)))) (->v3 0 0.3 0))]
      (if (> (count (every ball)) 80) (destroy! (last (every ball))))
      (throb (parent! (clone! :ball point) (the ground)))))

(defn -update [o] (when (key-down? "p") (destroy! (every ball))))


(defn start-demo [o] 
  (clear-cloned!)
  (mapv destroy! (every ball))
  (log (count (every ball)))
  (hook+ (clone! :ground) :update #'tween.test/-update )
  (hook+ (the ground-plane) :on-mouse-over #'tween.test/click))

(defn j [^UnityEngine.GameObject o]
  (fn []
    (set! (.position (.transform o)) 
      (Vector3. 
        (.x (.position (.transform o))) 
        (Math/Sin (+ Time/time (.x (.position (.transform o))) )) 
        (.z (.position (.transform o)))))))

(defn joe [^UnityEngine.GameObject o] 
  (set! (.position (.transform o)) 
      (Vector3. 
        (.x (.position (.transform o))) 
        (Math/Sin (+ Time/time 0.1)) 
        (.z (.position (.transform o))))))

'(let [n (clear-cloned!)
  ground (clone! :ground)] 
  (dorun (for [x (range 30) z (range 30)]
       (timeline [(j (clone! :ball (->v3 x 0 z)))]))))

'(let [n (clear-cloned!)
  ground (clone! :ground)
  ball (hook+ (clone! :ball) :update #'tween.test/joe)] 
  (dorun (for [x (range 30) z (range 30)]
       (clone! ball (->v3 x 0 z)))))

'(let [n (clear-cloned!)
  ground (clone! :ground)] 
  (for [x (range 30) z (range 30)]
    (throb (clone! :ball (->v3 x 0 z)))))

'(let [n (clear-cloned!)
  ground (clone! :ground)] 
  (dorun (for [x (range 30) z (range 30)]
       (every-frame (j (clone! :ball (->v3 x 0 z)) (rand))))))