# tween
A fast tween library for arcadia/Unity

## implementation

* Tweening is implemented as a component for each tweenable property, this avoids garbage collection allowing high fps
* A proxy <Tween> deftype stores the specific values of a tween configuration, and can be used as a fn on gameobjects to initiate a tween.
* A constuctor fn creates a Tween for the appropriate component

# usage
```clj
(ns test.core
  (:require [tween.core :as tween]))

;defining a <Tween> via a constructor fn
(def my-move (tween/position (Vector3. -4 0 0) 2.5 :+))

;applying a <Tween> on an object
(my-move (GameObject.))
```

# Tweens are associative
```clj
(def z (tween/euler (Vector3. 45 0 100) 0.5))
(def y (assoc z :duration 1.2))
(= z y) ;false
```

# callbacks/chained tweens

<Tweens> can be given a callback function (takes 1 arg for the gameobject)
```clj
(def cb 
  (tween/position 
    (Vector3. 0 0 0) 0.5 
    (fn [go] (UnityEngine.Debug/Log (str "tween ended for " go)))))
```

and <Tweens> are suitable callbacks
```clj
(def b (tween/position (Vector3. 0 0 0) 0.5))
(def a (tween/position (Vector3. 0 -2 0) 0.5 b))
```

callbacks are atomic, to facilitate cyclical <Tween> chains
```clj
(assoc b :callback a)
;will now repeat
```

<Tweens> can be combined with comp
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
