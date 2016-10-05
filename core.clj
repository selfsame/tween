(ns tween.core
  (:import 
    [UnityEngine Application MonoBehaviour WaitForSeconds Time Mathf
     GameObject Color Vector4 Vector3 Vector2 Quaternion]
    [System Object]
    [System.Collections IEnumerator]
    Wait MonoObj

    ObjectPair SinglePair DoublePair Int16Pair Int32Pair Int64Pair QuaternionPair 
    Vector4Pair Vector3Pair Vector2Pair ColorPair)
  (:require [clojure.walk :as walk]))

(defonce THIS 'THYSELF)
(defonce REGISTRY (atom {}))
(def panic false)

(def ^System.Single -single (float 0.0))
(def ^System.Int32  -short  (int 0))
(def ^System.Boolean TRUE  true)
(def ^System.Boolean FALSE false)

(def -mono-obj (volatile! nil))
(defn mono-obj []
    (if (. Application isPlaying)
      (or (if (not (UnityEngine.Object/op_Equality @-mono-obj nil)) 
              @-mono-obj)
        (vreset! -mono-obj 
          (let [o (or (GameObject/Find "tween.core/-mono-obj")
                      (GameObject. "tween.core/-mono-obj"))]
            (or (.GetComponent o MonoObj) 
                (.AddComponent o MonoObj)))))))

(defn- datatype? [v] (or (sequential? v) (set? v) (map? v)))

(defn- symbol-replace [form xform]
  (cond (symbol? form)   (get xform form form)
        (datatype? form) (clojure.walk/walk #(symbol-replace % xform) identity form)
        :else form))

(defmacro deftween [path props methods]
  (let [this (first props)]
    `(do (swap! REGISTRY assoc ~path 
      (quote ~(into {} (map 
        (juxt first 
          (comp ;(partial list 'quote) 
                (comp #(symbol-replace % {this THIS}) last))) 
        methods))))
     (list (quote ~'tween) ~path))))

(defn unqualify [s] (last (re-seq #"[^\.]+" (str s))))
(defn var->qualified-symbol [v] (symbol (last (re-find #"^#'(.*)" (str v)))))

(defmacro deftag [tag methods]
  (let [entry (conj methods {:tag tag})]
    `(do (swap! REGISTRY assoc (quote ~tag) (quote ~entry))
      (list (quote ~'tag) (quote ~tag)))))

(defmacro AND [& more]
  (let [syms (take (count more) (repeatedly gensym))]
   `(~'let [~@(mapcat vector syms more)] 
      (~'fn [] (~'and ~@(map list syms))))))

(defmacro OR [& more]
  (let [syms (take (count more) (repeatedly gensym))]
   `(~'let [~@(mapcat vector syms more)] 
      (~'fn [] (~'or ~@(map list syms))))))

(defmacro NOT [f]
  (let [sym (gensym)]
   `(~'let [~sym ~f] 
      (~'fn [] (~'not (~sym))))))



(defn ^Wait wait [n] (Wait. n))


(deftype TimeLineCursor [
  ^:volatile-mutable ^System.Int32    i 
  ^:volatile-mutable ^boolean         v])

(defn timeline [fns]
  (when (. Application isPlaying)
    (let [current (volatile! (first fns))
          fns (volatile! (rest fns))
          ^UnityEngine.MonoBehaviour coro-root (mono-obj)
          ^tween.core.TimeLineCursor cursor (TimeLineCursor. 0 false)
          routine
      (.StartCoroutine coro-root
        (reify IEnumerator
          (MoveNext [this]
            (when-not panic
              (try 
                (set! (.v cursor) (.invoke @current))
                (if (.v cursor) TRUE
                  (if (do (vreset! current (first @fns) )
                          (vswap! fns rest) @current)
                    TRUE FALSE))
                (catch Exception e false))))
          (get_Current [this] (.v cursor))))]
      (fn [] (if @current routine false)))))

(defn timeline-1 [fns] (timeline (map #(%) fns)))

(defn array-timeline [^clojure.lang.PersistentHashSet opts
                      ^|System.Object[]|               fns]
  (when (. Application isPlaying)
    (let [cnt (int (dec (.Length fns)))
          current (volatile! ((aget fns 0)))
          ^UnityEngine.MonoBehaviour coro-root (mono-obj)
          ^tween.core.TimeLineCursor cursor (TimeLineCursor. 0 false)
          routine 
      (.StartCoroutine coro-root
        (reify IEnumerator
          (MoveNext [this]
            (try 
              (set! (.v cursor) (.invoke @current))
              (if (.v cursor) TRUE
                (if (>= (.i cursor) cnt) 
                    (if (opts :loop) 
                      (do (set! (.i cursor) 0)
                          (vreset! current ((aget fns 0))) TRUE)
                      FALSE)
                    (do (set! (.i cursor) (inc (.i cursor)))
                        (vreset! current ((aget fns (.i cursor))))
                        @current)))
              (catch Exception e FALSE)))
          (get_Current [this] (.v cursor))))]
      (fn [] (if (.v cursor) routine FALSE)))))

(defmacro timeline* [& fns]
  (let [[opts fns] ((juxt (comp set filter) remove) keyword? fns)
        ar (gensym)]
    `(~'let [~ar (~'make-array System.Object ~(count fns))] 
      ~@(map-indexed #(list 'aset ar %1 (list 'fn [] %2)) fns)
      (array-timeline ~opts ~ar))))



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

(defn args->opts [m]
  (into {} (mapv 
    #(cond (map? %) %
           (= :+ %) {:+ true}
           (#{:pow2 :pow3 :pow4 :pow5} %) {:in % :out %}) m)))


(defn ^System.Boolean expired? [^Wait w] (if (.invoke w) TRUE FALSE))

(deftype ^:once TweenCursor [
  ^:volatile-mutable ^boolean          initiated
  ^:volatile-mutable ^System.Single    start 
  ^:volatile-mutable ^System.Single    duration
  ^:volatile-mutable ^System.Single    ratio
  ^:volatile-mutable ^System.Single    now])

(defmacro tween [m o d & more]
  (let [opts      (args->opts more)
        ratio-code '(.ratio cursor)
        easefn    (apply (partial compile-ease '(.ratio cursor))  ((juxt :in :out) opts))
        cursor    (with-meta 'cursor {:tag Wait} )
        paths     (map-paths m)
        prop-data (remove (comp nil? first) 
                    (map (juxt (comp @tween.core/REGISTRY first) last) paths))
        props     (map (comp (juxt :tag :get) first) prop-data)
        tags      (map first props)
       -bases     (map (comp (juxt :base :base-tag) first) prop-data)
        basemap   
        (into {} 
          (map 
            (fn [[code tag]]
              {code (if tag (with-meta (gensym (unqualify tag)) {:tag tag}) (gensym 'base))}) 
            (set (remove (comp nil? first) -bases))))
        bases     (map (comp basemap first) -bases)
        base-binds (mapcat (juxt last first) basemap)
        getters   (map last props)
        base-getters   (map (fn [b g] (if b (symbol-replace g {THIS b}) g)) bases getters)
        tagmaps   (map @REGISTRY tags)
        defaults  (map :identity tagmaps)
        targets   (mapv last prop-data)
        pairsyms  (mapv gensym (take (count props) (repeat 'p)))
        val-binds 
        (mapcat 
          #(vector 
            (with-meta %3 {:tag (:pair %2)}) 
            (list 'new (:pair %2) (:identity %2) %4)) 
          tags tagmaps pairsyms targets)]

   `(~'let [~(with-meta THIS {:tag UnityEngine.GameObject}) ~o
          ~cursor (~'Wait. (float ~d))
          ~@base-binds
          ~@val-binds]
      (~'fn []
        (~'if ~'(.active cursor) TRUE
          (~'do  
          ~@(map #(list 'set! (list '.a %1) %2) pairsyms base-getters)))
          ~@(map
             #(list 'set! %1
                (list (:lerp %2)
                  (list '.a (get pairsyms %3))
                  (list '.b (get pairsyms %3))
                  easefn ))
            base-getters tagmaps (range 100))
          (expired? ~'cursor)))))


(deftag System.Object          {:pair ObjectPair     :lerp (fn [a b _] b)           :identity (System.Object.)})
(deftag System.Single          {:pair SinglePair     :lerp Mathf/Lerp               :identity (float 0.0)})
(deftag System.Double          {:pair DoublePair     :lerp Mathf/Lerp               :identity (double 0.0)})
(deftag UnityEngine.Vector2    {:pair Vector2Pair    :lerp UnityEngine.Vector2/Lerp :identity (UnityEngine.Vector2.)})
(deftag UnityEngine.Vector3    {:pair Vector3Pair    :lerp UnityEngine.Vector3/Lerp :identity (UnityEngine.Vector3.)})
(deftag UnityEngine.Vector4    {:pair Vector4Pair    :lerp UnityEngine.Vector4/Lerp :identity (UnityEngine.Vector4.)})
(deftag UnityEngine.Color      {:pair ColorPair      :lerp UnityEngine.Color/Lerp   :identity (UnityEngine.Color.)})
(deftag UnityEngine.Quaternion {:pair QuaternionPair :lerp Quaternion/LerpUnclamped :identity (Quaternion.)})


(deftween [:position] [this]
  {:get  (.position this)
   :base (.transform this)
   :base-tag UnityEngine.Transform
   :tag UnityEngine.Vector3})

(deftween [:local :position] [this]
  {:get (.localPosition this)
   :base (.transform this)
   :base-tag UnityEngine.Transform
   :tag UnityEngine.Vector3})

(deftween [:local :scale] [this]
  {:get (.localScale this)
   :base (.transform this)
   :base-tag UnityEngine.Transform
   :tag UnityEngine.Vector3})

(deftween [:scale] [this]
  {:get (.localScale this)
   :base (.transform this)
   :base-tag UnityEngine.Transform
   :tag UnityEngine.Vector3})

(deftween [:rotation] [this]
  {:get (.rotation this)
   :base (.transform this)
   :base-tag UnityEngine.Transform
   :tag UnityEngine.Quaternion})

(deftween [:local :rotation] [this]
  {:get (.localRotation this)
   :base (.transform this)
   :base-tag UnityEngine.Transform
   :tag UnityEngine.Quaternion})

(deftween [:euler] [this]
  {:get (.eulerAngles this)
   :base (.transform this)
   :base-tag UnityEngine.Transform
   :tag UnityEngine.Vector3})

(deftween [:local :euler] [this]
  {:get (.localEulerAngles this)
   :base (.transform this)
   :base-tag UnityEngine.Transform
   :tag UnityEngine.Vector3})

(deftween [:material :color] [this]
  {:base (.material (.GetComponent this UnityEngine.Renderer))
   :base-tag UnityEngine.Material
   :get (.color this)
   :tag UnityEngine.Color})

(deftween [:light :color] [this]
  {:base (.GetComponent this UnityEngine.Light)
   :get (.color this)
   :tag UnityEngine.Color})

(deftween [:light :range] [this]
  {:base (.GetComponent this UnityEngine.Light)
   :get (.range this)
   :tag System.Single})

(comment 
(defn spit-pair [n t] 
  (spit (str "Assets/tween/pairs/" n ".cs") (str 
    "using UnityEngine;\n"
    "public class " n "{\n"
    "    public " t " a;\n"
    "    public " t " b;\n"
    "    public " n "(" t " A, " t " B){\n"
    "    a = A; b = B;\n  }\n}" )))

(map (partial apply spit-pair) [
  ['ObjectPair      'System.Object]
  ['SinglePair      'System.Single]
  ['DoublePair      'System.Double]
  ['Int16Pair       'System.Int16]
  ['Int32Pair       'System.Int32]
  ['Int64Pair       'System.Int64]
  ['QuaternionPair  'UnityEngine.Quaternion]
  ['Vector4Pair     'UnityEngine.Vector4]
  ['Vector3Pair     'UnityEngine.Vector3]
  ['Vector2Pair     'UnityEngine.Vector2]
  ['ColorPair       'UnityEngine.Color]]))

'[tween.core]