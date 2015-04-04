(ns tween.core
  (:require arcadia.core clojure.string)
  (:use tween.protocols)
  (:import [UnityEngine Color Vector3 GameObject Color]))


(defn- -V+ [^Vector3 a ^Vector3 b] (Vector3/op_Addition a b))

(def ^:private UID (atom 0))
(def REGISTRY (atom {}))
(declare callable?)

 
(defn finish [uid c]
  (when-let [f (get @REGISTRY uid)]
    ;(swap! REGISTRY dissoc uid)
    (mapv #(when (callable? %) (% c)) f))) 

(def ^:private easing 
  {:pow2 (fn [r] (* r r))
    :pow3 (fn [r] (* r r r))
    :pow4 (fn [r] (* r r r r))
    :pow5 (fn [r] (* r r r r r))
  :in (fn [f r] (f r))
  :out (fn [f r] (- 1 (f (- 1 r))))
  :inout (fn [fin fout r]
          (cond (< r 0.5) (/ (fin (* 2 r)) 2)
                  :else (- 1 (/ (fout (* 2 (- 1 r))) 2))))})

(def ^:private easeregistry
  (into {}
  (for [in [nil :pow2 :pow3 :pow4 :pow5]
      out [nil :pow2 :pow3 :pow4 :pow5]
      :let [infn (get easing in) 
            outfn (get easing out)]]
      (cond (= [in out] [nil nil]) {[in out] #(identity %)}
        (= out nil) {[in out] (fn [r] ((:in easing) infn r))}
        (= in nil) {[in out] #((:out easing) outfn %)}
        :else {[in out] #((:inout easing) infn outfn %)}

        ))))






(deftype Tween [^System.MonoType component opts callbacks]
  clojure.lang.IObj
  (ToString [this] (str "" component "|" (:duration opts)))
  (meta [this] (meta opts))

  clojure.lang.ILookup
  (valAt [_ k] (if (= k :callback) @callbacks (get opts k)))
  (valAt [_ k nf] (if (= k :callback) @callbacks (get opts k nf)))


  clojure.lang.Associative
  (assoc [this k v]
    (if (= k :callback)
      (do (reset! callbacks v) (if (sequential? this) this [this]))
      (Tween. component (assoc opts k v) (atom @callbacks))))

  clojure.lang.IPersistentCollection
  (equiv [self o]
    (if (instance? Tween o)
      (and (= component (.component o))
         (= opts (.opts o))
         (= @callbacks @(.callbacks o)))
      false))

  tween.protocols.Chainable
  (-link! [a b] 
    (when (callable? b)
          (swap! (.callbacks a) #(vec (conj (set %) b))))
          b)
  (-unlink! [a b]
    (if (or (instance? Tween b) (instance? clojure.lang.IFn b))
      (swap! (.callbacks a) #(vec (disj (set %) b)))))
  (-links [a] (.callbacks a))

  clojure.lang.IFn
  (invoke [this go] 
    (let [pre-c (.GetComponent go component) 
        c (or pre-c (.AddComponent go component))]
      (set! (.active c) true)
      (set! (.value c) (.target c))
      (if pre-c 
        (do (set! (.value c) ((.getfn c) go)))
        (do (set! (.value c)  ((.getfn c) go))))

      (set! (.uid c) (int (swap! UID inc)))
      (if @callbacks
        (swap! REGISTRY conj {(.uid c) @callbacks})
        (swap! REGISTRY dissoc (.uid c)))
      
      (set! (.start c) (- (+ (UnityEngine.Time/time) (:delay opts)) (.delay c)))
      (set! (.duration c) (:duration opts))
      (set! (.delay c) 0.0)

      (if (:+ opts) 
        (do ;(set! (.relative c) true)
            (set! (.target c) ((.addfn c) (.value c) (:target opts))))
        (set! (.target c) (:target opts)))
      (when-not (= (.easesig c) [(:in opts) (:out opts)]) 
        (set! (.easesig c) [(:in opts) (:out opts)])
        (set! (.easefn c) (get easeregistry [(:in opts)(:out opts) ])))
      go)))

(defn callable? [x] (if 
  (or (instance? Tween x) (fn? x)) true false))

(defn links [a] (when (instance? Tween a) (-links a)))
(defn unlink! [a b] (-unlink! a b))
(defn link! [a b & more]
  (when b
    (when (instance? Tween a)
      (-link! a b)
      (when more (apply link! (cons b more))))))



(defn make [c target & more]
  (let [args (conj {:target target :duration 1.0 :+ false :callback nil :delay 0.0}
        (into {} (mapv 
          #(cond 
            (= :+ %) {:+ true}
            (number? %) {:duration %}
            (map? %) %
            (#{:pow2 :pow3 :pow4 :pow5} %) {:in % :out %}
            (callable? %) {:callback [%]}) 
        more)))
        ]
  (Tween. c (dissoc args :callback) (atom (:callback args)))))


(defn- sort-methods [code]
  (if (sequential? code)
    (case (first code)
      -get {:getter (rest code)}
      -set {:setter (rest code)}
      -value {:valuer (rest code)}
      -add {:adder (cons 'fn (rest code))}
      -subtract {:subtractor (cons 'fn (rest code))}
      {}) {}))

(def ^{:private true} default-methods 
  {:getter [['this] ()]
    :valuer [['this] ()]
    :setter nil
    :adder '(fn [a b] (Vector3/op_Addition a b))
    :subtractor '(fn [a b] (Vector3/op_Subtraction a b))
    })

(defmacro deftween [sym props & methods ] ;getter valuer & setter
  (let [{:keys [getter valuer setter adder]} (conj default-methods (into {} (map sort-methods methods)))
        compsym (symbol (str sym "_tween"))
        res-props (concat '[^boolean active ^System.Double delay ^System.Double start ^System.Double duration ^boolean relative ^float ratio ^int uid
                           getfn addfn easefn easesig] props)
        [[this] get-more] getter
        [[this-v] value-more] valuer
        getterfn (cons 'fn getter)
        set-more (or setter (list 'set! get-more value-more))]
`(do

  (arcadia.core/defcomponent ~compsym ~res-props
   (~'Awake [~this]
      (set! (.getfn ~this) ~getterfn)
      (set! (.addfn ~this) ~adder)
      (set! (.duration ~this) 5.0)
      (set! (.start ~this) (UnityEngine.Time/time))
      (set! (.value ~this) ~get-more)
      (set! (.target ~this) ~get-more))
   (~'Update [~this-v]

      (if (.active ~this-v)
            (if  (> (- (UnityEngine.Time/time) (.start ~this-v)) 
                    (.duration ~this-v))

              (do (set! (.ratio ~this-v) (float 1.0))
                ~set-more
                (set! (.active ~this-v) false)
                (set! (.delay ~this-v) 
                  (- (- (UnityEngine.Time/time) (.start ~this-v)) 
                     (.duration ~this-v)))
                (tween.core/finish (.uid ~this-v) (.gameObject ~this-v)))
              
              (do
                (set! (.ratio ~this-v) 
                  (float ((.easefn ~this)
                   (/ (- (UnityEngine.Time/time) (.start ~this-v)) 
                      (.duration ~this-v)) )))
                 ~set-more)) ))
  (~'OnDestroy [~'this]
      (swap! tween.core/REGISTRY dissoc (.uid ~'this))))

  (defn ~sym ~'[& more] (apply tween.core/make (cons ~compsym ~'more)))

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
  (-add [a b] (Color/op_Addition a b))
  (-get [this] (.color (.material (.GetComponent this UnityEngine.Renderer))))
  (-value [this] (Color/Lerp (.value this) (.target this) (.ratio this))))

(deftween text-color [^Color value ^Color target]
  (-add [a b] (Color/op_Addition a b))
  (-get [this] (.color (.GetComponent this "TextMesh")))
  (-value [this] (Color/Lerp (.value this) (.target this) (.ratio this))))

(deftween light-color [^Color value ^Color target]
  (-add [a b] (Color/op_Addition a b))
  (-get [this] (.color (.GetComponent this "Light")))
  (-value [this] (Color/Lerp (.value this) (.target this) (.ratio this))))

(deftween light-range [^float value ^float target]
  (-get [this] (.range (.GetComponent this "Light")))
  (-value [this] (float (+ value (* (- target value) (.ratio this))))))

(deftween material-texture-offset [^Vector2 value ^Vector2 target] 
  (-get [this] (.GetTextureOffset (.material (.GetComponent this UnityEngine.Renderer) "_MainTex")))
  (-value [this] )
  (-set [this] (.SetTextureOffset (.material (.GetComponent this UnityEngine.Renderer)) 
                                  "_MainTex" 
                                  (Vector2/Lerp (.value this) (.target this) (.ratio this)))))



 