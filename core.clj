(ns tween.core
  (:import Vector3
    [UnityEngine Resources]
    [System GC]
    MonoBehaviour IEnumerator WaitForSeconds Time Mathf)
  (:require 
    [clojure.walk :as walk]
    [clojure.pprint :as pprint]
    [pdfn.core :refer [ppexpand]])
  (:use 
    pool
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





(def ^:private garbage (volatile! 0))
(defn take-out-trash! [] 
  (when (> (- Time/time @garbage) 0.5) 
        (log "garbage!")
        (System.GC/Collect) 
        (vreset! garbage Time/time)) true)
;(defonce wednesday (every-frame #(take-out-trash!)))

(deftype TimeLineCursor [
  ^:volatile-mutable ^System.Int64 i 
  ^:volatile-mutable ^boolean      v
  ^:volatile-mutable ^boolean active])

(def-pool 1000 TimeLineCursor ^System.Int64 i v active)

;(deftype Fns [^:volatile-mutable ^|System.Object[]| arr])
;(def-pool 100 Fns  ^|System.Object[]| arr)
;(.arr (*Fns (make-array Object 10) ))

(defn f [& more] (into-array System.Object more))


(fn vec->arr [v ^|System.Object[]| arr]
  (loop [i 0 cnt (.Length arr)]
    (aset arr i (get v i))))

(defn timeline [-fns]
  (let [^|System.Object[]| fns (into-array System.Object -fns)
        ^MonoBehaviour coro-root (mono-obj)
        ^TimeLineCursor cursor (*TimeLineCursor 0 false true)]
    (dorun (map-indexed #(aset fns %1 %2) -fns))
    (.StartCoroutine coro-root
      (reify IEnumerator
        (MoveNext [this]
          (let [res (try ((aget fns (.i cursor)))
              (catch Exception e false))]
            (if (= res :break) 
                (do (set! (.i cursor) (.Length fns) )
                    (set! (.v cursor) false))
                (set! (.v cursor) res)))
          (if (.v cursor) true
            (if (< (.i cursor) (- (.Length fns) 1)) 
                (set! (.i cursor) (+ (.i cursor) 1))
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
  ^:volatile-mutable ^System.Single    duration])

(def-pool 1000 TweenCursor initiated start duration)

(deftype Pair       [^:volatile-mutable a b])
(deftype Pair-v3    [^:volatile-mutable ^Vector3 a ^Vector3 b])
(deftype Pair-color [^:volatile-mutable ^UnityEngine.Color a ^UnityEngine.Color b])
(def pair-types {
  'UnityEngine.Vector3 'tween.core.Pair-v3
  'UnityEngine.Color   'tween.core.Pair-color})

(defmacro tween [m o & more]
  (let [opts      (args->opts more)
        easefn    (list 'get 'tween.core/easeregistry ((juxt :in :out) opts))
        this      (with-meta THIS {} #_{:tag 'UnityEngine.GameObject} )
        cursor    (with-meta 'cursor {:tag 'tween.core.TweenCursor} )
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
   `(~'let [~this ~o
            ~cursor (~'*TweenCursor false ~'Time/time ~(:duration opts))
            ~@val-binds]
      (~'fn []
        (~'when-not ~'(.initiated cursor) 
        ~'(set! (.initiated cursor) true)
        ~'(set! (.start cursor) Time/time)
        ~@(map #(list 'set! (list '.a %1) %2) pairsyms getters))
      ~@(map-indexed
        #(list 'set! (:get (first %2)) (list (:lerp (first %2))
          (list '.a (get pairsyms %1))
          (list '.b (get pairsyms %1))
          (list 'Mathf/InverseLerp 0.0 
            '(.duration cursor) 
            '(- Time/time (.start cursor)))))
        prop-data)
      ~'(if (< (- Time/time (.start cursor)) (.duration cursor))
          true
          (!TweenCursor cursor)) ))))




(pprint/print-table   (mapv #(do {:pool %1 :stats (stats %2)}) 
  '[<>WaitCursor <>TimeLineCursor <>TweenCursor]
  [<>WaitCursor <>TimeLineCursor <>TweenCursor]))

(deftween [:position] [this]
  {:get (.position (.transform this))
   :lerp Vector3/Lerp
   :tag UnityEngine.Vector3})

(deftween [:local :position] [this]
  {:get (.localPosition (.transform this))
   :lerp Vector3/Lerp
   :tag UnityEngine.Vector3})



(deftween [:local :scale] [this]
  {:get (.localScale (.transform this))
   :lerp Vector3/Lerp
   :tag UnityEngine.Vector3})

(deftween [:euler] [this]
  {:get (.eulerAngles (.transform this))
   :lerp Vector3/Lerp
   :tag UnityEngine.Vector3})

(deftween [:local :euler] [this]
  {:get (.localEulerAngles (.transform this))
   :lerp Vector3/Lerp
   :tag UnityEngine.Vector3})

(deftween [:material-color] [this]
  {:get (.color this)
   :lerp UnityEngine.Color/Lerp
   :tag UnityEngine.Color})






(deftween [:material :color] [this]
  {:get (.color (.material (.GetComponent this UnityEngine.Renderer)))
   :lerp UnityEngine.Color/Lerp
   :tag UnityEngine.Color})


(ppexpand (tween {:euler (->v3 1 2 3)} (clone! :ball) 2.0))