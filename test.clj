(ns tween.test 
  (:require tween.core))

(tween.core/deftag UnityEngine.Vector2 {
  :lerp UnityEngine.Vector2/Lerp           
  :identity (UnityEngine.Vector2.)})

(tween.core/deftween [:box-collider-2d :offset] [this]
  {:get (.offset this)
   :tag UnityEngine.Vector2
   :base (.GetComponent this UnityEngine.BoxCollider2D)
   :base-tag UnityEngine.BoxCollider2D})

(let [o (GameObject. "tween.test")]
  (.AddComponent o UnityEngine.BoxCollider2D)
  (tween.core/timeline* :loop
    (tween.core/wait 0.5)
    (tween.core/tween {:box-collider-2d {:offset (Vector2. (rand) (rand))}} o 0.3)))