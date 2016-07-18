# tween.core
A fast tween library for arcadia/Unity

## timelines


`timeline` starts a coroutine that iterates a `seq?` collection. Items are invoked every frame and lazily iterate when falsey. 

```clj
(timeline [
 #(log "hello")
  (wait 1.0)
 #(log "goodbye")])
```


`timeline-1` uses another fn layer to provide an initialization closure.

```clj
(timeline-1 
  (cycle [
    (fn [] (wait (rand)))
    (fn [] #(log 'tick)])))
```

`timeline*` macro wrapps forms with `(fn [])`, and uses an Array for performance. Keywords are removed for options. `:loop` cycles the iterator.

```clj
(timeline* :loop
  (wait 0.5)
  (tween {:local {:scale (->v3 (rand))}} 
   (the ball) 0.9))
```

### wait

`(wait 0.5)`

returns fn that returns true for the duration after it's first invokation


## tweens

## `deftag` macro

Registers a type for tweening

```clj
(deftag       UnityEngine.Vector3 
  {:lerp      UnityEngine.Vector3/Lerp           
   :identity  (UnityEngine.Vector3.)})
```

## `deftween` macro

register an edn path with property access. The path is arbitrary and the base object is user provided on usage.

```clj
(deftween [:local :scale] [this]
  {:get (.localScale (.transform this))
   :tag UnityEngine.Vector3})
```

## `tween` macro

Expands into a fn that returns true for the duration after it's first invokation. 

*  `arg-1`  map of target values
*  `arg-2`  object reference
*  `arg-3`  duration
*  `& more` compile into opt map

The target map is exploded into paths, a path registered with `deftween` will expand into code.


```clj
(tween 
 {:local 
   {:scale (Vector3. 2.0 2.0 2.0)
    :position (Vector3. 0.0 2.0 0.0)}}  
  (GameObject.) 
  2.5 {:in :pow5})
```

### easing fns

`{:in :pow3}`

*  keys `:in` `:out`
*  vals `:pow2` `:pow3` `:pow4` `:pow5` `nil`
