(ns plankton.core
  (:gen-class))

(use 'mikera.image.core)
(use 'mikera.image.colours)
(use 'mikera.image.filters)

(def image-path "test-image.jpg")

(def plankton-img (load-image image-path))

(def pixels (get-pixels plankton-img))

(defn average-pixel-value [pixels]
  (/ (reduce + 0 pixels) (count pixels)))


(defn threshold-pixels [pixels]
  (let [av (average-pixel-value pixels)]
    (map
     #(if (> %  av)
        127
        0)
     pixels)
    ))

(defn seq-map-byte-array [f b] (byte-array (f
     (seq b))))

(defn pixelfilter [f image] (let [pixels (get-pixels image)]
                              (set-pixels image (seq-map-byte-array f pixels)))
  )

(defn threshold [image] (pixelfilter threshold-pixels image))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (threshold plankton-img)
  (show plankton-img :zoom 5.0))

(def plak-pix (get-pixels plankton-img))

(def test-average (average-pixel-value plak-pix))


