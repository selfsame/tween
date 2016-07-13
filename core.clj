(ns tween.core
  (:import Vector3
    [UnityEngine Resources]
    [System GC]
    MonoBehaviour IEnumerator WaitForSeconds Time)
  (:require [clojure.walk :as walk]
    [clojure.pprint :as pprint]
    [pdfn.core :refer [ppexpand]])
  (:use 
    arcadia.core
    hard.core
    hard.input))

(def ^:private REGISTRY (atom {}))
(def ^:private THIS (gensym 'this))

(defn mono-obj [] 
  (or (object-typed MonoBehaviour) 
      (hook+ (GameObject. "tween.core/mono-obj") :start log)))

(defn every-frame [f]
  (let [coro-root (mono-obj)]
    (.StartCoroutine coro-root
      (reify IEnumerator
        (MoveNext [this] (f))
        (get_Current [this])))))

(def ^:private easing {
  :pow2   (fn [r] (* r r))
  :pow3   (fn [r] (* r r r))
  :pow4   (fn [r] (* r r r r))
  :pow5   (fn [r] (* r r r r r))
  :in     (fn [f r] (f r))
  :out    (fn [f r] (- 1 (f (- 1 r))))
  :inout  (fn [fin fout r]
            (cond (< r 0.5) (/ (fin (* 2 r)) 2)
                    :else (- 1 (/ (fout (* 2 (- 1 r))) 2))))})

(def easeregistry
  (into {}
    (for [in  [nil :pow2 :pow3 :pow4 :pow5]
          out [nil :pow2 :pow3 :pow4 :pow5]
          :let [infn  (get easing in) 
                outfn (get easing out)]]
        (cond (= [in out] [nil nil]) {[in out] #(identity %)}
          (= out nil)                {[in out] (fn [r] ((:in easing) infn r))}
          (= in nil)                 {[in out] #((:out easing) outfn %)}
          :else                      {[in out] #((:inout easing) infn outfn %)}))))


(defn map-paths 
  ([m] (map-paths [] m))
  ([res m]
    (if (map? m) 
      (mapcat #(map-paths (conj res %) (get m %)) (keys m))
      [[res m]])))

(defn- datatype? [v] (or (sequential? v) (set? v) (map? v)))

(defn- symbol-replace [form xform]
  (cond (symbol? form)   (get xform form form)
        (datatype? form) (clojure.walk/walk #(symbol-replace % xform) identity form)
        :else form))

(defmacro deftween [path props methods]
  (let [this# (first props)]
  `(swap! REGISTRY assoc ~path 
    (quote ~(into {} 
      (map (juxt first (comp (comp #(symbol-replace % {this# THIS}) last))) methods))))))



(deftype WaitCursor [
  ^:volatile-mutable ^System.Single start 
  ^:volatile-mutable ^boolean       initiated])

(defn wait [n]
  (let [cursor (WaitCursor. 0 false)] 
    (fn [] 
      (when-not (.initiated cursor) 
        (set! (.initiated cursor) true)
        (set! (.start cursor) Time/time))
      (pos? (- (+ (.start cursor) n) Time/time)))))

(def ^:private garbage (volatile! 0))
(defn take-out-trash! [] 
  (when (> (- Time/time @garbage) 0.5) 
        (log "garbage!")
        (System.GC/Collect) 
        (vreset! garbage Time/time)) true)
'(defonce wednesday (every-frame #(take-out-trash!)))

(deftype TimeLineCursor [
  ^:volatile-mutable ^int     i 
  ^:volatile-mutable ^boolean v
  ^:volatile-mutable ^boolean active])

(defn timeline [& fns]
  (let [que (volatile! fns)
        ^MonoBehaviour coro-root (mono-obj)
        ^TimeLineCursor cursor (TimeLineCursor. 0 false true)]
    (.StartCoroutine
      coro-root
      (reify IEnumerator
        (MoveNext [this]
          (set! (.v cursor) (try ((get fns (.i cursor))) (catch Exception e nil)))
          (if (.v cursor) nil
              (set! (.i cursor) (inc (.i cursor))))
          (or (< (.i cursor) (count fns)) 
              (set! (.active cursor) false)))
        (get_Current [this] (.v cursor))))
    (fn [] (.active cursor))))

(defn args->opts [m]
  (into {} (mapv 
    #(cond (map? %) %
           (= :+ %) {:+ true}
           (#{:pow2 :pow3 :pow4 :pow5} %) {:in % :out %}) m)))



(deftype TweenCursor [
  ^:volatile-mutable ^boolean          initiated
  ^:volatile-mutable ^System.Single    start 
  ^:volatile-mutable ^System.Single    ratio])

(deftype Pair       [^:volatile-mutable a b])
(deftype Pair-v3    [^:volatile-mutable ^Vector3 a ^Vector3 b])
(deftype Pair-color [^:volatile-mutable ^UnityEngine.Color a ^UnityEngine.Color b])
(def pair-types {
  'UnityEngine.Vector3 'tween.core.Pair-v3
  'UnityEngine.Color   'tween.core.Pair-color})

(defmacro tween [m & more]
  (let [opts      (args->opts more)
        easefn    (list 'get 'tween.core/easeregistry ((juxt :in :out) opts))
        this      (with-meta THIS {:tag UnityEngine.GameObject})
        duration  (with-meta 'duration {:tag System.Double})
        cursor    (with-meta 'cursor {:tag tween.core.TweenCursor})
        paths     (map-paths m)
        prop-data (remove (comp nil? first) 
                    (map (juxt (comp @REGISTRY first) last) paths))
        props     (map (comp (juxt :tag :get) first) prop-data)
        tags      (map first props)
        getters   (map last props)
        targets   (mapv last prop-data)
        pairsyms  (mapv gensym (take (count props) (repeat 'Pair)))
        val-binds 
        (mapcat 
          #(vector 
            (with-meta %2 {:tag (get pair-types %1 'tween.core.Pair)}) 
            (list 'new (get pair-types %1 'Pair) %3 %4)) 
          tags pairsyms getters targets)]
   `(~'fn [~this ~duration]     
      (~'let [~cursor ~'(tween.core.TweenCursor. false Time/time 0.0)
              ~@val-binds]
        (~'fn []
          (~'when-not ~'(.initiated cursor) 
          ~'(set! (.initiated cursor) true)
          ~'(set! (.start cursor) Time/time)
          ~@(map #(list 'set! (list '.a %1) %2) pairsyms getters))
        ~'(set! (.ratio cursor) 
            (- 1 (/ (- duration (- Time/time (.start cursor))) duration)))
        ~@(map-indexed
          #(list 'set! (:get (first %2)) (list (:lerp (first %2))
            (list '.a (get pairsyms %1))
            (list '.b (get pairsyms %1))
            (list easefn '(.ratio cursor)))) 
          prop-data)
        ~'(< (- Time/time (.start cursor)) duration) )))))

(deftween [:position] [this]
  {:get (.position (.transform this))
   :lerp Vector3/Lerp
   :tag UnityEngine.Vector3})

(deftween [:local-scale] [this]
  {:get (.localScale (.transform this))
   :lerp Vector3/Lerp
   :tag UnityEngine.Vector3})

(deftween [:material :color] [this]
  {:get (.color (.material (.GetComponent this UnityEngine.Renderer)))
   :lerp UnityEngine.Color/Lerp
   :tag UnityEngine.Color})


