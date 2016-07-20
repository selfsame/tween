(ns tween.core
  (:import Vector3 
    [UnityEngine Application HideFlags MonoBehaviour
     GameObject Color Vector4 Vector3 Vector2 Quaternion  
     WaitForSeconds Time Mathf]
    [System GC Object]
    [System.Collections IEnumerator]
    Timeline)
  (:require 
    [clojure.walk :as walk])
  (:use tween.pool))

(defonce REGISTRY (atom {}))
(defonce THIS (gensym 'this))
(def panic false)

(def ^System.Single -single (float 0.0))
(def ^System.Int32  -short  (int 0))

(def -mono-obj (volatile! nil))
(defn mono-obj []
    (if (. Application isPlaying)
      (or (if (not (UnityEngine.Object/op_Equality @-mono-obj nil)) 
              @-mono-obj)
        (vreset! -mono-obj 
          (let [o (or (GameObject/Find "tween.core/-mono-obj")
                      (GameObject. "tween.core/-mono-obj"))] 
            ;(set! (.hideFlags o) HideFlags/HideInHierarchy)
            (or (.GetComponent o Timeline) 
                (.AddComponent o Timeline)))))))

(defn every-frame [f]
  (if (. Application isPlaying) 
    (let [coro-root (mono-obj)]
      (.StartCoroutine coro-root
        (reify IEnumerator
          (MoveNext [this] (f))
          (get_Current [this]))))))

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

(defn unqualify [s] (last (re-seq #"[^\.]+" (str s))))
(defn var->qualified-symbol [v] (symbol (last (re-find #"^#'(.*)" (str v)))))

(defmacro deftag [tag methods]
  (let [nss (str *ns*)
        sym (symbol (unqualify tag))
        pair-sym (symbol (str "Pair-" sym))
        wm-f #(with-meta % {:tag tag :volatile-mutable true})
        [T P C R] (map (comp symbol str) [nss nss nss nss] ["." "/" "/" "/"] ["" "<>" "*" "!"] (repeat pair-sym))
        entry (conj methods {:pair {:tag T :vars {:p P :c C :r R}}})]
    
    `(do 
      (deftype ^:once ~pair-sym [~(wm-f 'a) ~(wm-f 'b)])
     ~(do (swap! REGISTRY assoc tag entry) true)
      (def-pool 10000 ~pair-sym ~'a ~'b)
      (quote ~entry))))



(deftype WaitCursor [
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



(deftype TimeLineCursor [
  ^:volatile-mutable ^System.Int32    i 
  ^:volatile-mutable ^boolean         v])

(def-pool 1000 TimeLineCursor i v)

(def <over> (*TimeLineCursor 0 false))

(defn timeline [fns]
  (when (. Application isPlaying)
    (let [current (volatile! (first fns))
          fns (volatile! (rest fns))
          ^UnityEngine.MonoBehaviour coro-root (mono-obj)
          ^tween.core.TimeLineCursor cursor (*TimeLineCursor 0 false)
          routine
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
          (get_Current [this] (.v cursor))))]
      (fn [] (if @current routine false)))))

(defn timeline-1 [fns] (timeline (map #(%) fns)))

(defn array-timeline [^clojure.lang.PersistentHashSet opts
                      ^|System.Object[]|               fns]
  (when (. Application isPlaying)
    (let [cnt (int (dec (.Length fns)))
          current (volatile! ((aget fns 0)))
          ^UnityEngine.MonoBehaviour coro-root (mono-obj)
          ^tween.core.TimeLineCursor cursor (*TimeLineCursor 0 false)
          routine 
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
          (get_Current [this] (.v cursor))))]
      (fn [] (if (.v cursor) routine false)))))

(defmacro timeline* [& fns]
  (let [[opts fns] ((juxt (comp set filter) remove) keyword? fns)
        ar (gensym)]
    `(~'let [~ar (~'make-array System.Object ~(count fns))] 
      ~@(map-indexed #(list 'aset ar %1 (list 'fn [] %2)) fns)
      (array-timeline ~opts ~ar))))





(deftag System.Object          {:lerp (fn [a b _] b)                        :identity (System.Object)})
(deftag System.Single          {:lerp Mathf/Lerp                            :identity (float 0.0)})
(deftag System.Double          {:lerp Mathf/Lerp                            :identity (double 0.0)})
(deftag UnityEngine.Vector3    {:lerp UnityEngine.Vector3/Lerp              :identity (UnityEngine.Vector3.)})
(deftag UnityEngine.Color      {:lerp UnityEngine.Color/Lerp                :identity (UnityEngine.Color.)})
(deftag UnityEngine.Quaternion {:lerp UnityEngine.Quaternion/LerpUnclamped  :identity (UnityEngine.Quaternion.)})



(defn args->opts [m]
  (into {} (mapv 
    #(cond (map? %) %
           (= :+ %) {:+ true}
           (#{:pow2 :pow3 :pow4 :pow5} %) {:in % :out %}) m)))




(deftype ^:once TweenCursor [
  ^:volatile-mutable ^boolean          initiated
  ^:volatile-mutable ^System.Single    start 
  ^:volatile-mutable ^System.Single    duration
  ^:volatile-mutable ^System.Single    ratio
  ^:volatile-mutable ^System.Single    now])

(def-pool 10000 TweenCursor initiated start duration ratio now)

(defmacro tween [m o d & more]
  (let [opts      (args->opts more)
        ratio-code '(Mathf/InverseLerp 0.0 (.duration cursor) (.now cursor))
        easefn    (apply (partial compile-ease '(.ratio cursor))  ((juxt :in :out) opts))
        cursor    (with-meta 'cursor {:tag TweenCursor} )
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
            (with-meta %3 {:tag (or (-> %2 :pair :tag) *Pair-Object)}) 
            (list (or (-> %2 :pair :vars :c) *Pair-Object) (:identity %2) %4)) 
          tags tagmaps pairsyms targets)]

   `(~'let [~THIS ~o
          ~cursor (*TweenCursor false ~'Time/time (float ~d) -single -single)
          ~@base-binds
          ~@val-binds]
      (~'fn []
        (~'when-not ~'(.initiated cursor) 
        ~'(set! (.initiated cursor) true)
        ~'(set! (.start cursor) Time/time)
        ~@(map #(list 'set! (list '.a %1) %2) pairsyms base-getters))
        ~'(set! (.now cursor) (- Time/time (.start cursor)))
        (~'set! ~'(.ratio cursor) ~ratio-code)
      ~@(map
         #(list 'set! %1
            (list (:lerp %2)
              (list '.a (get pairsyms %3))
              (list '.b (get pairsyms %3))
              easefn ))
        base-getters tagmaps (range 100))
        (~'if ~'(< (.now cursor) (.duration cursor))
          true
          (~'do 
            ;~@(map #(list (or (-> %1 :pair :vars :r) !Pair-Object) %2) tagmaps pairsyms)
            ;(!TweenCursor ~'cursor)
            nil))))))




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

'[tween.core]



(comment 
(defn pool-report [] 
  (pprint/print-table   (mapv #(do {:pool %1 :stats (try (stats %2) (catch Exception e :error))}) 
    (into '[<>WaitCursor <>TimeLineCursor <>TweenCursor]
      (map (comp :p :pool :pair last) (filter (comp symbol? first) @REGISTRY)))
    (into [<>WaitCursor <>TimeLineCursor <>TweenCursor]
      (map (comp deref resolve :p :pool :pair last) (filter (comp symbol? first) @REGISTRY)))))))

