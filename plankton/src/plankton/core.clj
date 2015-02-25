(ns plankton.core
  (:gen-class))

(use 'mikera.image.core)
(use 'mikera.image.colours)

(def image-path "test-image.jpg")

(def plankton-img (load-image image-path))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (show plankton-img))

