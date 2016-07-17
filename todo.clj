(ns tween.todo)

;TODO stuffs
'[perf
  ((x) tween update)
  (( ) timeline construction
    (( ) macro timeline))]

'[design
  (( ) lazy timeline
    (( ) it is desirable to have logic inside the timeline above the fn layer)
         the entire timeline evals at it's initial application
         consuming the que as a lazy seq would allow control forms to access the tempo)
  ]



'(def T 
  (fn [o cursor]
    (lerp o A B ratio)))

'(let [cursor (*cursor)]
  (or 
    (fn []
      (if init (set! cursor start))
      (set! cursor ratio)
      (T [o cursor])
      (not over))
    (!cursor cursor)))
