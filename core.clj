(ns tween.core
  (:import Vector3 
    [UnityEngine Application HideFlags MonoBehaviour
     GameObject Color Vector4 Vector3 Vector2 Quaternion  
     WaitForSeconds Time Mathf]
    [System Object]
    [System.Collections IEnumerator]
    Timeline))

(def panic false)

(def -mono-obj (volatile! nil))
(defn mono-obj []
    (if (. Application isPlaying)
      (or (if (not (UnityEngine.Object/op_Equality @-mono-obj nil)) 
              @-mono-obj)
        (vreset! -mono-obj 
          (let [o (or (GameObject/Find "tween.core/-mono-obj")
                      (GameObject. "tween.core/-mono-obj"))] 
            (or (.GetComponent o Timeline) 
                (.AddComponent o Timeline)))))))

(defn every-frame [f]
  (if (. Application isPlaying) 
    (let [coro-root (mono-obj)]
      (.StartCoroutine coro-root
        (reify IEnumerator
          (MoveNext [this] (f))
          (get_Current [this]))))))

(deftype WaitCursor [
  ^:volatile-mutable ^System.Single start 
  ^:volatile-mutable ^boolean       initiated])

(defn wait [n] 
  (let [cursor (WaitCursor. (float 0) false)] 
    (fn [] 
      (if (.initiated cursor)
        (if (pos? (- (+ (.start cursor) (float n)) Time/time)) 
          true
          false)
        (do 
          (set! (.initiated cursor) true)
          (set! (.start cursor) Time/time) true)))))


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
                (set! (.v cursor) (@current))
                (if (.v cursor) true
                  (if (do (vreset! current (first @fns) )
                          (vswap! fns rest) @current)
                    true
                    false))
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
              (set! (.v cursor) (@current))
              (if (.v cursor) true
                (if (>= (.i cursor) cnt) 
                    (if (opts :loop) 
                      (do (set! (.i cursor) 0)
                          (vreset! current ((aget fns 0))) true)
                      false)
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