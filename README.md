# tween
A tween library for arcadia/Unity

## tweens are implemented as components for each tweenable property

# usage
```clj
(ns test.core
	(:require [tween.core :as tween]))

;a tween-fn
(def my-move (tween/position (Vector3. -4 0 0) 2.5 :+))

(my-move (GameObject.))
```

# tweens are associative
```clj
(def z (tween/euler (Vector3. 45 0 100) 0.5))
(def y (assoc z :duration 1.2))
(= z y) ;false
```

# callbacks/chained tweens

tweens can be given a callback function (takes 1 arg for the gameobject)
```clj
(def cb 
  (tween/position 
    (Vector3. 0 0 0) 0.5 
    (fn [go] (UnityEngine.Debug/Log (str "tween ended for " go)))))
```

tween-fns are valid callbacks
```clj
(def b (tween/position (Vector3. 0 0 0) 0.5))
(def a (tween/position (Vector3. 0 -2 0) 0.5 b))
```

callbacks are atomic, to facilitate cyclical tween chains
```clj
(assoc b :callback a)
;will now repeat
```

tweens can be combined with comp
```clj
(def comp-tween (tween-a tween-b))
(comp-tween (GameObject.))
```

# custom tweens
```clj
(tween/deftween camera-size [^float value ^float target]
  (-get [this] (.fieldOfView (.GetComponent this "Camera")))
  (-value [this] (float (+ value (* (- target value) (.ratio this))))))

((camera-size 45 5.0 ) (UnityEngine.Camera/main))
```
