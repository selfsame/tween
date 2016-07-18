(ns tween.test
  (:use 
    tween.core
    pdfn.core
    arcadia.core
    arcadia.linear
    hard.core
    hard.input))


'(timeline (cycle [(wait 1.0) #(log Time/time)]))

;fn closure for initialization
'(timeline-1 (cycle [(fn [] (wait (rand))) ]))

;macro
'(timeline* :loop
  (wait 0.5)
  (tween {:local {:scale (->v3 (rand))}} 
   (the ball) 0.9))

'(timeline [
  (tween 
  {:light {
    :range (float (+ 15 (rand 50)))
    :color (color (rand-vec 1.0 1.0 1.0))}} 
    (the lamp) 1.0 :pow4)])


(defn g1 [^UnityEngine.GameObject o]
  (tween 
   {:local {
      :scale (Vector3. 2.0 2.0 2.0)
      :position (v3+ (.position (.transform o)) 
                     (V* (.* o.transform.position.normalized) 4.0))}} o 0.7 {:out :pow2}))

(defn test1 [^UnityEngine.Material m ^UnityEngine.GameObject o]
  (timeline* :loop
    (wait (?f 0.5 1.5))
    (tween {:material-color (UnityEngine.Color. 1 0 0)} m 0.5 {:in :pow3})
    (g1 o)
    (tween 
       {:local {
          :scale (Vector3. 1.0 1.0 1.0)
          :position (.position (.transform o)) }} o 0.7 {:in :pow2})
    (tween {:material-color (UnityEngine.Color. 1 1 1)} m 0.5 {:in :pow3})))


(defn start-test [_] 
  (clear-cloned!)
  (let [root (clone! :empty)
        globe (parent! (local-scale! (clone! :ball) (->v3 40)) root)] 
    (hook+ root :update #(rotate! % (Vector3. 0.0 0.1 0.0)))
    (timeline [(tween {:material-color (UnityEngine.Color. 0.0 0.6 0.08)} 
                  (.* globe>Renderer.material) 0.1)])
  (dorun (for [x (range 15) 
               z (range 15)
               :let [o (parent! (clone! :ball (?on-sphere 20)) root)]]
    (test1 (.* o>Renderer.material) o)))) )



(time (start-test nil))

