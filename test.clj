(ns tween.test 
  (:require tween.core))

(tween.core/deftag UnityEngine.Vector2 {
  :lerp UnityEngine.Vector2/Lerp           
  :identity (UnityEngine.Vector2.)})

(tween.core/deftween [:box-collider-2d :offset] [this]
  {:get (.offset (.GetComponent this UnityEngine.BoxCollider2D))
   :tag UnityEngine.Vector2})

(defn t1 [] 
  (let [o (GameObject. "tween.test")]
    (.AddComponent o UnityEngine.BoxCollider2D)
    (tween.core/timeline* :loop
      (tween.core/wait 0.5)
      (tween.core/tween {:box-collider-2d {:offset (Vector2. (rand) (rand))}} o 0.3))))
