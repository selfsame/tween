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

todo