(ns tween.core
  (:import Vector3
    [UnityEngine Resources]
    [System GC]
    MonoBehaviour IEnumerator WaitForSeconds Time Mathf)
  (:require 
    [clojure.walk :as walk]
    arcadia.repl
  [clojure.pprint :as pprint] )
  (:use 
    pool
    pdfn.core
    arcadia.core
    arcadia.linear
    hard.core
    hard.input))

(defonce REGISTRY (atom {}))
(defonce THIS (gensym 'this))

(def -mono-obj (volatile! nil))
(defn mono-obj []
  (or @-mono-obj
    (vreset! -mono-obj   
      (or (object-typed MonoBehaviour) 
        (hook+ (GameObject. "tween.core/mono-obj") :start #'arcadia.core/log)))))

(defn every-frame [f]
  (let [coro-root (mono-obj)]
    (.StartCoroutine coro-root
      (reify IEnumerator
        (MoveNext [this] (f))
        (get_Current [this])))))

(def garbage (volatile! 0))
(def garbage-active (volatile! false))
(defn take-out-trash! [] 
  (when (and @garbage-active (> (- Time/time @garbage) 0.7)) 
        (.Enqueue arcadia.repl/work-queue 
          ["(do (in-ns 'tween.core) (System.GC/Collect))" nil nil])
        (vreset! garbage Time/time)) true)
(defn wednesday [] (if (vswap! garbage-active not) (every-frame take-out-trash!)))


(defn- datatype? [v] (or (sequential? v) (set? v) (map? v)))

(defn- symbol-replace [form xform]
  (cond (symbol? form)   (get xform form form)
        (datatype? form) (clojure.walk/walk #(symbol-replace % xform) identity form)
        :else form))

(defmacro pow2 [a] `(Mathf/Pow ~a 2))
(defmacro pow3 [a] `(Mathf/Pow ~a 3))
(defmacro pow4 [a] `(Mathf/Pow ~a 4))
(defmacro pow5 [a] `(Mathf/Pow ~a 5))

(def quote-ease {
  :pow2   'pow2
  :pow3   'pow3
  :pow4   'pow3
  :pow5   'pow3
  :linear 'n
  :in     '(i n)
  :out    '(- 1 (o (- 1 n)))
  :inout  '(if (< n 0.5) 
             (/ (i (* 2 n)) 2)
             (- 1 (/ (o (* 2 (- 1 n))) 2)))})

(defn compile-ease [rsym ik ok]
  (let [[i o] (mapv quote-ease [ik ok])
         bk (cond (every? nil? [o i]) :linear
                 (nil? o) :in 
                 (nil? i) :out
                 :else    :inout)
         b (get quote-ease bk)]
       (symbol-replace b {'n rsym 'i i 'o o})))

(defn map-paths 
  ([m] (map-paths [] m))
  ([res m]
    (if (map? m) 
      (mapcat #(map-paths (conj res %) (get m %)) (keys m))
      [[res m]])))




(defmacro deftween [path props methods]
  (let [this (first props)]
    (swap! REGISTRY assoc path 
      (into {} (map 
        (juxt first (comp (comp #(symbol-replace % {this THIS}) last))) 
        methods)))
    `~path))

(defmacro deftag [tag methods]
  (let [sym (symbol (last (re-seq #"[^\.]+" (str tag))))
        pair-sym (symbol (str "Pair-" sym))
        wm-f #(with-meta % {:tag tag :volatile-mutable true})
        pool-m (zipmap [:p :c :r] (map (comp symbol str) ["<>" "*" "!"] (repeat pair-sym)))
        entry (conj methods {:pair {:tag pair-sym :pool pool-m}})]
    (swap! REGISTRY assoc tag entry)
    `(do 
      (deftype ~pair-sym [~(wm-f 'a) ~(wm-f 'b)])
      (def-pool 10000 ~pair-sym ~'a ~'b)
      (quote ~entry))))



(deftype WaitCursor [
  ^:volatile-mutable ^System.Single start 
  ^:volatile-mutable ^boolean       initiated])

(def-pool 1000 WaitCursor start initiated)

(defn wait [n] 
  (let [cursor (*WaitCursor 0 false)] 
    (fn [] 
      (when-not (.initiated cursor) 
        (set! (.initiated cursor) true)
        (set! (.start cursor) Time/time))
      (if (pos? (- (+ (.start cursor) n) Time/time)) 
        true
        (!WaitCursor cursor)))))



(deftype TimeLineCursor [
  ^:volatile-mutable ^System.Int64    i 
  ^:volatile-mutable ^boolean         v
  ^:volatile-mutable ^System.Single overage])

(def-pool 1000 TimeLineCursor ^System.Int64 i v overage)

(def <over> (*TimeLineCursor 0 false 0.0))

(defn timeline [fns]
  (let [cnt (count fns)
        ^MonoBehaviour coro-root (mono-obj)
        ^TimeLineCursor cursor (*TimeLineCursor 0 false 0.0)]
    (.StartCoroutine coro-root
      (reify IEnumerator
        (MoveNext [this]
          (set! (.v cursor) 
                (try ((get fns (.i cursor)))
                     (catch Exception e false)))
          (if (.v cursor) true
            (if (<= (.i cursor) cnt) 
                (set! (.i cursor) (inc (.i cursor)))
                (!TimeLineCursor cursor))))
        (get_Current [this] (.v cursor))))
    (fn [] false)))



(defn args->opts [m]
  (into {:duration 0.5} (mapv 
    #(cond (map? %) %
           (number? %) {:duration %}
           (= :+ %) {:+ true}
           (#{:pow2 :pow3 :pow4 :pow5} %) {:in % :out %}) m)))




(deftype TweenCursor [
  ^:volatile-mutable ^boolean          initiated
  ^:volatile-mutable ^System.Single    start 
  ^:volatile-mutable ^System.Single    duration
  ^:volatile-mutable ^System.Single    ratio
  ^:volatile-mutable ^System.Single    now])

(def-pool 10000 TweenCursor initiated start duration ratio now)

(defmacro tween [m o & more]
  (let [opts      (args->opts more)
        easefn    (apply (partial compile-ease '(.ratio cursor))  ((juxt :in :out) opts))
        cursor    (with-meta 'cursor {:tag 'tween.core.TweenCursor} )
        paths     (map-paths m)
        prop-data (remove (comp nil? first) 
                    (map (juxt (comp @tween.core/REGISTRY first) last) paths))
        props     (map (comp (juxt :tag :get) first) prop-data)
        tags      (map first props)
        getters   (map last props)
        tagmaps   (map @REGISTRY tags)
        defaults  (map :identity tagmaps)
        targets   (mapv last prop-data)
        pairsyms  (mapv gensym (take (count props) (repeat 'p)))
        val-binds 
        (mapcat 
          #(vector 
            (with-meta %3 {:tag (or (-> %2 :pair :tag) '*Pair-Object)}) 
            (list (or (-> %2 :pair :pool :c) '*Pair) (:identity %2) %4)) 
          tags tagmaps pairsyms targets)]
   `(~'let [~THIS ~o
            ~cursor (~'*TweenCursor false ~'Time/time ~(:duration opts) 0.0 0.0)
            ~@val-binds]
      ;[~tags ~tagmaps]
      (~'fn []
        (~'when-not ~'(.initiated cursor) 
        ~'(set! (.initiated cursor) true)
        ~'(set! (.start cursor) (- Time/time (.overage <over>)) )
        ~@(map #(list 'set! (list '.a %1) %2) pairsyms getters))
        ~'(set! (.now cursor) (- Time/time (.start cursor)))
        ~'(set! (.ratio cursor) 
                (Mathf/InverseLerp 0.0 (.duration cursor) (.now cursor)))
      ~@(map
         #(list 'set! (:get (first %1)) 
            (list (:lerp %2)
              (list '.a (get pairsyms %3))
              (list '.b (get pairsyms %3))
              easefn))
        prop-data tagmaps (range 100))
        (~'if ~'(< (.now cursor) (.duration cursor))
          true
          (~'do 
            ~@(map #(list (or (-> %1 :pair :pool :r) '!Pair-Object) %2) tagmaps pairsyms)
            ~'(set! (.overage <over>) (- (.now cursor) (.duration cursor)))
            ~'(!TweenCursor cursor)
            false) )))))

(deftag System.Object       {:lerp (fn [a b _] b)                     :identity (System.Object)})
(deftag System.Single       {:lerp Mathf/Lerp                         :identity (float 0.0)})
(deftag System.Double       {:lerp Mathf/Lerp                         :identity (double 0.0)})
(deftag UnityEngine.Vector3 {:lerp UnityEngine.Vector3/Lerp           :identity (UnityEngine.Vector3.)})
(deftag UnityEngine.Color   {:lerp UnityEngine.Color/Lerp             :identity (UnityEngine.Color.)})


(deftween [:position] [this]
  {:get (.position (.transform this))
   :tag UnityEngine.Vector3})

(deftween [:local :position] [this]
  {:get (.localPosition (.transform this))
   :tag UnityEngine.Vector3})

(deftween [:local :scale] [this]
  {:get (.localScale (.transform this))
   :tag UnityEngine.Vector3})

(deftween [:scale] [this]
  {:get (.scale (.transform this))
   :tag UnityEngine.Vector3})

(deftween [:euler] [this]
  {:get (.eulerAngles (.transform this))
   :tag UnityEngine.Vector3})

(deftween [:local :euler] [this]
  {:get (.localEulerAngles (.transform this))
   :tag UnityEngine.Vector3})

(deftween [:material-color] [this]
  {:get (.color this)
   :tag UnityEngine.Color})

(deftween [:material :color] [this]
  {:get (.color (.material (.GetComponent this UnityEngine.Renderer)))
   :tag UnityEngine.Color})

(deftween [:light :color] [this]
  {:get (.* this >Light.color)
   :tag UnityEngine.Color})

(deftween [:light :range] [this]
  {:get (.* this >Light.range)
   :tag System.Single})








(defn pool-report [] 
  (pprint/print-table   (mapv #(do {:pool %1 :stats (stats %2)}) 
    (into '[<>WaitCursor <>TimeLineCursor <>TweenCursor]
      (map (comp :p :pool :pair last) (filter (comp symbol? first) @REGISTRY)))
    (into [<>WaitCursor <>TimeLineCursor <>TweenCursor]
      (map (comp deref resolve :p :pool :pair last) (filter (comp symbol? first) @REGISTRY))))))

(print (pool-report))











(def run true)

(defn hue [^UnityEngine.Material m ^UnityEngine.GameObject o]
  (timeline [
    (wait (rand))
    (tween {:local 
      {:scale (Vector3. 2.0 2.0 2.0)
       :position (v3+ (.position (.transform o)) 
                      (Vector3. 0.0 2.0 0.0))}} o 1.0 {:in :pow5})
    (tween {:material-color (UnityEngine.Color. (?f) (?f) (?f))} m (float 0.5) {:in :pow3})
    (tween {:local 
      {:scale (Vector3. 1.0 1.0 1.0)
       :position (v3+ (.position (.transform o)) 
                      (Vector3. 0.0 0.0 0.0))}} o (float (rand 0.5)) :pow5)
    
    #(do (hue m o) nil)]))


(defn start-demo [_] 
  (clear-cloned!)
  (dorun (for [x (range 40) 
               z (range 40)
               :let [o (clone! :ball (V* (->v3 x 0 z) 2.0)) ]]
    (hue (.material (.GetComponent o UnityEngine.Renderer)) o))) )

(start-demo nil)


(timeline [ 
(tween 
  {:light {
    :range (float (rand 50))
    :color (color (rand-vec 1.0 1.0 1.0))}} 
    (the lamp) 0.5 :pow4)])

'(ppexpand     (tween {
      :scale (Vector3. 2.0 2.0 2.0)
      :position (v3+ (.position (.transform (the lamp))) 
                     (Vector3. 0.0 2.0 0.0))} (the lamp) 1.0 {:in :pow5}))

