(ns tween.pool (:import [UnityEngine]))

(defprotocol IPool
  (reuse   [a])
  (recycle [a b])
  (stats   [a]))

(deftype Pool [^|System.Object[]|                pool 
               ^System.Int64 ^:volatile-mutable  idx]
  IPool
  (stats [a]
    (list (inc idx) '/ (.Length pool)))
  (reuse [a]
    (try 
      (set! idx (dec idx))
      (aget pool (inc idx))
      (catch Exception e (set! idx (inc idx)) nil)))
  (recycle [a b]
    (try 
      (set! idx (inc idx)) 
      (aset pool idx b) nil
      (catch Exception e  (set! idx (dec idx)) nil))))

(defmacro def-pool [length type-sym & fields]
  (let [sym (with-meta (symbol (str "*" type-sym)) {:tag type-sym})
        pool (with-meta (symbol (str "<>" type-sym)) {:tag type-sym})
        return (with-meta (symbol (str "!" type-sym)) {:tag System.Boolean})
        o# (gensym)]
    `(~'let []
      (~'def ~pool (new ~Pool (~'make-array ~'System.Object ~length) -1))
      (~'defn ~return
        [~(with-meta (symbol "a") {:tag type-sym})] 
        (recycle ~pool ~'a))
      (~'defn ~sym [~@fields]
        (~'if-let [~(with-meta o# {:tag type-sym}) (reuse ~pool)]
          (~'do 
          ~@(map #(list 'set! (list (symbol (str "." %)) o#) %) 
              fields) ~o#)
        (new ~type-sym ~@fields)))
      ~(mapv keyword [pool sym return]))))