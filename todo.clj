(ns tween.todo
  (:require [clojure.walk :as walk]
    [clojure.pprint :as pprint]
    [pdfn.core :refer [ppexpand]])
  (:use 
    arcadia.core
    hard.core
    hard.input
    tween.core
    pool)
  (:import [UnityEngine Vector3]
    selfsame))

;TODO stuffs
'[perf
  (( ) discover tween fn form that doesn't alloc memory)]

'[design
  (( ) lazy timeline
    (( ) it is desirable to have logic inside the timeline above the fn layer)
         the entire timeline evals at it's initial application
         consuming the que as a lazy seq would allow control forms to access the tempo)
  ]




(def running (volatile! false))

(deftype Tween [^:volatile-mutable ^Vector3 from ^:volatile-mutable ^Vector3 to])
(def-pool 100000 Tween ^Vector3 from ^Vector3 to)
(ppexpand (def-pool 100000 Tween ^Vector3 from ^Vector3 to))

(!Tween (Tween. (Vector3.) (Vector3.) ))
(!Tween (Tween. (Vector3.) (Vector3.) ))

(defn work [] 
  (when @running 
      #_(dotimes [i 1000] 
        (do 
          (Tween.  (Vector3.) (Vector3.))
          ;(!tween* (tween* (Vector3.) (Vector3.) ))
          ))  
     ; (!tween* (tween* (Vector3.) (Vector3.) ))
     (dotimes [i 1000]  
     ;(Tween.  (Vector3.) (Vector3.))
     (!Tween (*Tween (Vector3.) (Vector3.) ))
      ) true))

(defn bench []
  (vreset! running true)
  (every-frame (fn [] (work))))


(vswap! running not)


(bench)