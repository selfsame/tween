(ns tween.todo
  (:require [clojure.walk :as walk]
    [clojure.pprint :as pprint]
    [pdfn.core :refer [ppexpand]]
    )
  (:use 
    arcadia.core
    hard.core
    hard.input
    tween.core)
  (:import [UnityEngine Vector3]))

(defn foo [^System.String s] (str s s))

;TODO stuffs

'[perf
  (( ) discover tween fn form that doesn't alloc memory)]

'[design
  (( ) lazy timeline
    (( ) it is desirable to have logic inside the timeline above the fn layer)
         the entire timeline evals at it's initial application
         consuming the que as a lazy seq would allow control forms to access the tempo)
  ]

(deftype Frog [^System.String name ^System.Int32 age])

'(deftype Fish [])




(def POOL-CTORS (atom {}))

(defmacro def-pool [sym type-sym & fields]
  (let [pool# (get @POOL-CTORS type-sym (get (swap! POOL-CTORS assoc type-sym (gensym sym)) type-sym ))]
     
    `(do 
      (~'def ~pool# (~'make-array ~'System.Object 1000))
      (~'defn ~sym [~@fields]
        (~'aset ~pool# 1 
          (new ~type-sym ~@fields))))
    ))



'(ppexpand (def-pool frog Frog ^System.String name age))

(def-pool frog Frog ^System.String name age)
@POOL-CTORS

'(map #(aset frog22160 % (Frog. "sam" %)) (take 400 (repeatedly #(rand-int 999))))



'(let [ar frog22160] 
(every-frame (fn []
(count 
(loop [res (list) idx 0]
  (if (<= (.Length ar) idx) res
      (recur 
        (if (aget ar idx) 
            (cons (aget ar idx) res) 
            res)
        (inc idx))))))))