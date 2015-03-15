(ns tween.core
  (:require arcadia.core clojure.string)
  (:import [UnityEngine Color Vector3 GameObject Color]))


;TODO
;[ ] deftween fn for +'ing target and value for relative target
;[ ] link! protocol for adding callbacks
;[ ] callback should handle sequences
;[ ] delay functionality (in link! ?)
;[ ] tweens should call finish with remainder float, if a callback is a tween add remainder to start time
;[x] when recycling tween comp or destroying, make sure uid is dissoc'd from REGISTRY

(defn V+ [^Vector3 a ^Vector3 b] (Vector3/op_Addition a b))
(defn V- [^Vector3 a ^Vector3 b] (Vector3/op_Subtraction a b))
(defn V* [a b] (Vector3/op_Multiply a b))
(defn V÷ [a b] (Vector3/op_Division a b))
(defn Vx [^Vector3 a ^Vector3 b] (Vector3/Cross a b))

(def ^:private UID (atom 0))
(def ^:private REGISTRY (atom {}))
(def ^:private PROP-GETS (atom {}))

(defn ^:private finish [uid c]
  (when-let [f (get @REGISTRY uid)]
    (swap! REGISTRY dissoc uid)
    (f c))) 

(deftype Tween [^System.MonoType component target opts callbacks]
  clojure.lang.IObj
  (ToString [this] (str "" component "|" (:duration opts)))
  (meta [this] (meta opts))

  clojure.lang.ILookup
  (valAt [_ k] (if (= k :callback) @callbacks (get opts k)))
  (valAt [_ k nf] (if (= k :callback) @callbacks (get opts k nf)))


  clojure.lang.Associative
  (assoc [this k v]
    (if (= k :callback)
      (do (reset! callbacks v) this)
      (Tween. component target (assoc opts k v) (atom @callbacks))))

  clojure.lang.IPersistentCollection
  (equiv [self o]
    (if (instance? Tween o)
      (and (= component (.component o))
         (= target (.target o))
         (= opts (.opts o)))
      false))

  clojure.lang.IFn
  (invoke [this go] 
    (let [pre-c (.GetComponent go component) 
        c (or pre-c (add-component go component))
        uid (int (swap! UID inc))]
      (when pre-c 
        (swap! REGISTRY dissoc (.uid c))
        (set! (.value c) (.target c)))
      (when @callbacks
        (swap! REGISTRY conj {uid @callbacks}))
      (set! (.uid c) uid)
      (set! (.start c) (float (UnityEngine.Time/time)))
      (set! (.duration c) (float (:duration opts)))
      
      (when-let [getter (get @PROP-GETS (last (clojure.string/split (str component) #"\.")))]
        (set! (.value c)  (getter go)))
      (if (:relative opts)
        (set! (.target c) (V+ (.value c) target))
        (set! (.target c) target))
      go)) )

(defn make [c target & more]
  (let [args (conj {:duration 1.0 :relative false :callback nil}
        (into {} (mapv 
          #(cond 
            (= :+ %) {:relative true}
            (number? %) {:duration %}
            (instance? clojure.lang.IFn %) {:callback %}) 
        more)))]
  (Tween. c target (dissoc args :callback) (atom (:callback args)))))





(defmacro deftween [sym props getter valuer & setter]
  (let [compsym (symbol (str sym "_comp"))
        res-props (concat '[^float start ^float duration ^boolean relative ^float ratio ^int uid] props)
        [_ [this] get-more] getter
        [_ [this-v] value-more] valuer
        set-more (if (first setter) (first (drop 2 (first setter))) (list 'set! get-more value-more))]
  `(~'do
    (~'swap! ~'PROP-GETS ~'conj {(~'str (~'quote ~sym)) (~'fn [~this] ~get-more)})
  (~'arcadia.core/defcomponent ~compsym ~res-props
   (~'Awake [~this]
    ('~use '~'hard.core)
      (~'set! (~'.duration ~this) (float 5.0))
      (~'set! (~'.start ~this) (~'UnityEngine.Time/time))
      (~'set! (~'.value ~this) ~get-more)
      (~'set! (~'.target ~this) ~get-more))
   (~'Update [~this-v]
      (~'if (~'> (~'- (~'UnityEngine.Time/time) (~'.start ~this-v)) (~'.duration ~this-v))
          (~'do 
            (~'finish (~'.uid ~this-v) (~'.gameObject ~this-v))) 
          (~'do
            (~'set! (~'.ratio ~this-v) (~'float (~'/ (~'- (~'UnityEngine.Time/time) (~'.start ~this-v)) (~'.duration ~this-v))))
            ~set-more)))
  ~'(OnDestroy [this]
    (swap! REGISTRY dissoc (.uid this))) )
  (~'defn ~sym [~'& ~'more] (~'apply ~'make (~'cons ~compsym ~'more)))
  )))
 




(deftween position [^Vector3 value ^Vector3 target]
  (-get [this] (.position (.transform this)))
  (-value [this] (Vector3/Lerp value target (.ratio this))))

(deftween scale [^Vector3 value ^Vector3 target]
  (-get [this] (.localScale (.transform this)))
  (-value [this] (Vector3/Lerp (.value this) (.target this) (.ratio this))))

(deftween euler [^Vector3 value ^Vector3 target]
  (-get [this] (.eulerAngles (.transform this)))
  (-value [this] (Vector3/Lerp (.value this) (.target this) (.ratio this))))

(deftween material-color [^Color value ^Color target]
  (-get [this] (.color (.material (.GetComponent this UnityEngine.Renderer))))
  (-value [this] (Color/Lerp (.value this) (.target this) (.ratio this))))

(deftween material-texture-offset [^Vector2 value ^Vector2 target] 
  (-get [this] (.GetTextureOffset (.material (.GetComponent this UnityEngine.Renderer) "_MainTex")))
  (-value [this] )
  (-set [this] (.SetTextureOffset (.material (.GetComponent this UnityEngine.Renderer)) 
                                  "_MainTex" 
                                  (Vector2/Lerp (.value this) (.target this) (.ratio this)))))








; (def t (make position (Vector3. -5 0 0) 0.5 ))
; (def tt (make position (Vector3. 5 0 0) 0.5 t))
; (assoc t :callback tt)

; (def rt (make position (Vector3. -1 0 0) 0.5 :+))
; (def e (make euler (Vector3. 0 90 0) 0.5  :+))
; (assoc rt :callback (comp rt e))


; (defn rand-pos [] (make position (Vector3. (rand)(rand)(rand)) (rand) :+ #((rand-pos) %)))


; (defn test-tweens [dim]
;   (clear-cloned)
;   (destroy! (find-name "trash"))
;   (def trash (or (active) (create-primitive :sphere)))
;   (set! (.name trash) "trash")
;   (add-component trash t-position)

;   (doall (for [x (range dim) y (range dim)] 
;            (let [n (clone! trash (v* [x 0 y] 2))]
;              ((rand-pos) n)
;              ))))