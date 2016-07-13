(ns tween.todo
  (:gen-class))

(defn foo [^System.String s] (str s s))

;TODO stuffs

'[perf
  (( ) discover tween fn form that doesn't alloc memory)]

'[design
  (( ) lazy timeline
    (( ) it is desirable to have logic inside the timeline above the fn layer)
         the entire timeline evals at it's initial application
         consuming the que as a lazy seq would allow control forms to access the tempo)
  ]
