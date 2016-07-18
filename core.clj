(ns tween.core
  (:import Vector3 
    [UnityEngine Color]
    [UnityEngine Resources]
    [System GC]
    MonoBehaviour IEnumerator WaitForSeconds Time Mathf)
  (:require 
    [clojure.walk :as walk])
  (:use 
    tween.pool
    arcadia.core))


(defonce REGISTRY (atom {}))
(defonce THIS (gensym 'this))
(def panic false)

(def -mono-obj (volatile! nil))
(defn mono-obj []
  (if-not (null-obj? @-mono-obj) @-mono-obj
    (vreset! -mono-obj   
      (or (object-typed MonoBehaviour) 
          (hook+ (GameObject. "tween.core/mono-obj") :start #'arcadia.core/log)))))

(defn every-frame [f]
  (let [coro-root (mono-obj)]
    (.StartCoroutine coro-root
      (reify IEnumerator
        (MoveNext [this] (f))
        (get_Current [this])))))


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
        once (gensym (str "_once_" sym))
        pair-sym (symbol (str "Pair-" sym))
        qualified-pair-sym (symbol (str "tween.core.Pair-" sym))
        wm-f #(with-meta % {:tag tag :volatile-mutable true})
        pool-m (zipmap [:p :c :r] (map (comp symbol str) ["<>" "*" "!"] (repeat pair-sym)))
        entry (conj methods {:pair {:tag qualified-pair-sym :pool pool-m}})]
    
    `(do 
        (defonce ~once 
          (do 
           ~(do (swap! REGISTRY assoc tag entry) true)
            (deftype ^:once ~pair-sym [~(wm-f 'a) ~(wm-f 'b)])))
        (def-pool 10000 ~pair-sym ~'a ~'b)
        (quote ~entry))))



(deftype ^:once WaitCursor [
  ^:volatile-mutable ^System.Single start 
  ^:volatile-mutable ^boolean       initiated])

(def-pool 1000 WaitCursor start initiated)

(defn wait [n] 
  (let [cursor (*WaitCursor (float 0) false)] 
    (fn [] 
      (if (.initiated cursor)
        (if (pos? (- (+ (.start cursor) (float n)) Time/time)) 
          true
          (!WaitCursor cursor))
        (do 
          (set! (.initiated cursor) true)
          (set! (.start cursor) Time/time) true)))))



(deftype ^:once TimeLineCursor [
  ^:volatile-mutable ^System.Int64    i 
  ^:volatile-mutable ^boolean         v
  ^:volatile-mutable ^System.Single overage])

(def-pool 1000 TimeLineCursor ^System.Int64 i v overage)

(def <over> (*TimeLineCursor 0 false 0.0))

(defn timeline [fns]
  (let [current (volatile! (first fns))
        fns (volatile! (rest fns))
        ^MonoBehaviour coro-root (mono-obj)
        ^TimeLineCursor cursor (*TimeLineCursor 0 false 0.0)]
    (.StartCoroutine coro-root
      (reify IEnumerator
        (MoveNext [this]
          (when-not panic
            (try 
              (set! (.v cursor) (@current))
              (if (.v cursor) true
                (if (do (vreset! current (first @fns) )
                        (vswap! fns rest) @current)
                  true
                  (!TimeLineCursor cursor)
                  ))
              (catch Exception e false))))
        (get_Current [this] (.v cursor))))
    (fn [] (if @current true false))))

(defn timeline-1 [fns] (timeline (map #(%) fns)))

(defn array-timeline [^clojure.lang.PersistentHashSet opts
                      ^|System.Object[]|               fns]
  (let [cnt (dec (.Length fns))
        current (volatile! ((aget fns 0)))
        ^MonoBehaviour coro-root (mono-obj)
        ^TimeLineCursor cursor (*TimeLineCursor 0 false 0.0)]
    (.StartCoroutine coro-root
      (reify IEnumerator
        (MoveNext [this]
          (try 
            (set! (.v cursor) (@current))
            (if (.v cursor) true
              (if (>= (.i cursor) cnt) 
                  (if (opts :loop) 
                    (do (set! (.i cursor) 0)
                        (vreset! current ((aget fns 0))) true)
                    (!TimeLineCursor cursor))
                  (do (set! (.i cursor) (inc (.i cursor)))
                      (vreset! current ((aget fns (.i cursor))))
                      @current)))
            (catch Exception e false)))
        (get_Current [this] (.v cursor))))
    (fn [] (.v cursor))))

(defmacro timeline* [& fns]
  (let [[opts fns] ((juxt (comp set filter) remove) keyword? fns)
        ar (gensym)]
    `(~'let [~ar (~'make-array ~'System.Object ~(count fns))] 
      ~@(map-indexed #(list 'aset ar %1 (list 'fn [] %2)) fns)
      (~'array-timeline ~opts ~ar))))








(defn args->opts [m]
  (into {:duration 0.5} (mapv 
    #(cond (map? %) %
           (number? %) {:duration %}
           (= :+ %) {:+ true}
           (#{:pow2 :pow3 :pow4 :pow5} %) {:in % :out %}) m)))




(deftype ^:once TweenCursor [
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
          ~cursor (~'*TweenCursor false ~'(float Time/time) ~(:duration opts) (float 0.0) (float 0.0))
          ~@val-binds]
      (~'fn []

        (~'when-not ~'(.initiated cursor) 
        ~'(set! (.initiated cursor) true)
        ;~'(set! (.start cursor) (- Time/time (.overage <over>)) )
        ~@(map #(list 'set! (list '.a %1) %2) pairsyms getters))
        ~'(set! (.now cursor) (- Time/time (.start cursor)))
        ~'(set! (.ratio cursor) 
                (float (Mathf/InverseLerp 0.0 (.duration cursor) (.now cursor))))
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
            ;~'(set! (.overage <over>) (- (.now cursor) (.duration cursor)))
            ~'(!TweenCursor cursor)
            ;~'(set! (.initiated cursor) false)
            ))))))

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

'[tween.core]

(comment 
(defn pool-report [] 
  (pprint/print-table   (mapv #(do {:pool %1 :stats (try (stats %2) (catch Exception e :error))}) 
    (into '[<>WaitCursor <>TimeLineCursor <>TweenCursor]
      (map (comp :p :pool :pair last) (filter (comp symbol? first) @REGISTRY)))
    (into [<>WaitCursor <>TimeLineCursor <>TweenCursor]
      (map (comp deref resolve :p :pool :pair last) (filter (comp symbol? first) @REGISTRY)))))))



