# tween
A fast tween library for arcadia/Unity

![tween example](http://selfsamegames.com/gifs/img/bubb23.gif)

This is still experimental, feedback on overall design is welcome.

## implementation

* Tweening is implemented as a component for each tweenable property, this avoids garbage collection allowing high fps.  A tween channel will reuse an existing component, for best performance attach tweens to an object before cloning in game.
* A proxy ```<Tween>``` type stores the specific values of a tween configuration, and can be used as a fn on gameobjects to initiate a tween.
* each property channel has a constructor to create a proxy ```<Tween>```. ```scale``` is the constructor fn for the ```scale_tween``` component.

# usage
```clj
(ns test.core
  (:require [tween.core :as tween]))

;defining a <Tween> via a constructor fn
(def my-move (tween/position (Vector3. -4 0 0) 2.5 :+))

;applying a <Tween> on an object
(my-move (GameObject.))
```

## constructor args
A constructor takes a target value, and optional args with no order:
  * ```{}``` A map to merge with found options
  * ```0.7``` A float - ```{:duration 0.7}```
  * ```:+``` relative tween flag - ```{:+ true}```
  * ```:pow2 :pow3 :pow4 :pow5``` easing shorthand for same in and out - ```{:in :pow2 :out :pow2}```
  * ```#(%)``` a fn - ```{:callback (atom [#(%)])}```

# Tweens are semi-associative
```clj
(def z (tween/euler (Vector3. 45 0 100) 0.5))
(def y (assoc z :duration 1.2))
(= z y) ;false

(:duration z)
;0.5
```

# Tweens are sort of functions
```clj
(my-tween my-game-object)
(map my-tween my-col)
```


# callbacks/chained tweens

A Tween has an internal atom of callbacks. They are called after the tween has finished, and should take 1 arg - the gameobject the tween was attached to.

Callbacks can be added to the opts with ```:callback```. A fn in the constructor also works.

```clj
(def cb 
  (tween/position 
    (Vector3. 0 0 0) 0.5 
    (fn [go] (UnityEngine.Debug/Log (str "tween ended for " go)))))
```

```<Tweens>``` are suitable callbacks
```clj
(def b (tween/position (Vector3. 0 0 0) 0.5))
(def a (tween/position (Vector3. 0 -2 0) 0.5 b))
```

Callbacks can be also be configured with ```link! unlink! links```
```clj
(link! a b a)
(link! b z y)
;a - b - a - CYCLE
;       \ z - y
```


## currently provided tweens (```tween.core```)
```
position 
scale
euler 
material-color 
text-color 
light-color 
light-range 
material-texture-offset
```


# custom tweens
You can define new tween properties with the ```deftween``` macro.

```-get``` and ```-value``` are the basic required methods
```clj
(tween/deftween camera-size [^float value ^float target]
  (-get [this] (.fieldOfView (.GetComponent this "Camera")))
  (-value [this] (float (+ value (* (- target value) (.ratio this))))))

((camera-size 45 5.0 ) (UnityEngine.Camera/main))
```

The ```value``` and ```target``` prop args must be defined.

The macro will also define hinted props for 
```^boolean active ^float delay ^float start ^float duration ^boolean relative ^float ratio ^int uid```
as well as non-serialziable props for ```getfn addfn easefn``` which hold clojure fn's

To allow relative tweening, you must provide an ```-add``` method (it defaults to Vector3/op_Addition)

```clj
(deftween text-color [^Color value ^Color target]
  (-add [a b] (Color/op_Addition a b))
  (-get [this] (.color (.GetComponent this "TextMesh")))
  (-value [this] (Color/Lerp (.value this) (.target this) (.ratio this))))
 ```

Setting the tween value defaults to the form ```(set! ~-get ~-value)```. If a different form is needed, you can also define a ```-set``` method (note the -value method is now unused)

```clj
 (deftween material-texture-offset [^Vector2 value ^Vector2 target] 
  (-get [this] (.GetTextureOffset (.material (.GetComponent this UnityEngine.Renderer) "_MainTex")))
  (-add [a b] (Vector2/op_Addition a b))
  (-set [this] (.SetTextureOffset (.material (.GetComponent this UnityEngine.Renderer)) 
                                  "_MainTex" 
                                  (Vector2/Lerp (.value this) (.target this) (.ratio this)))))
 ```


