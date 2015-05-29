(ns plankton.imageprocessing)

(defrecord Image [bytes width height])

(defn average-pixel-value [pixels]
  (/ (reduce + 0 (map int pixels)) (count pixels)))

(defn threshold-pixels [image]
  (let [pixels (:bytes image)
        av (average-pixel-value pixels)]
    (map
     #(if (> %  av)
        255
        0)
     pixels)
    ))
