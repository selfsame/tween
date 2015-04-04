(ns tween.protocols)

(defprotocol Chainable
  (-link! [a b])
  (-unlink! [a b])
  (-links [a]))