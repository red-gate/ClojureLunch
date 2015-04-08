(ns plankton.core
  (:gen-class)
  (:import [java.awt.image BufferedImage BufferedImageOp])
  )

(use 'mikera.image.core)
(use 'mikera.image.colours)

(def image-path "test-image.jpg")

(def plankton-img (load-image image-path))

(def pixels (get-pixels plankton-img))

(defn average-pixel-value [pixels]
  (/ (reduce + 0 (map int pixels)) (count pixels)))


(defn threshold-pixels [pixels]
  (let [av (average-pixel-value pixels)]
    (map
     #(if (> %  av)
        255
        0)
     pixels)
    ))


(defn byte-mangling [b] (int (bit-and b 0xFF)))

(defn seq-map-byte-array [f b]
  (byte-array (f (seq b))))

(defn pixelfilter [f image]
  (let [pixels (get-pixels image)]
    (set-pixels image (seq-map-byte-array (comp f #(map byte-mangling %)) pixels))))

(defn threshold [image]
  (pixelfilter threshold-pixels image)
  image)


(defn flood-fill-from-index-with-colour [pixels index new-colour]
  (loop [pixels-to-check (list index)]
    (when (first pixels-to-check)
    (let [current (first pixels-to-check)]
      (when (not= (get pixels current) new-colour)
        (aset pixels current new-colour)
        ;; add neighbouhs
        )
      (recur (rest pixels-to-check)))))
  pixels)

(defn flood-fill-pixels [pixels]
  (let [enumerated-pixels (zipmap pixels (iterate inc 0))
        index (find enumerated-pixels 255)]
       (flood-fill-from-index-with-colour pixels index 100))
  )

(defn flood-fill-image [image]
   (pixelfilter flood-fill-pixels image))

(deftype Filter [^BufferedImageOp image-op]
  clojure.lang.IFn
    (invoke [this image]
      (let [^BufferedImage image image dest-img (.createCompatibleDestImage image-op image (.getColorModel image))]
        (.filter image-op image dest-img) dest-img))
    (applyTo [this args] (clojure.lang.AFn/applyToHelper this args)))

(defn dilate []
;  "Creates a simple blur filter (3x3 pixel) blur"
    (Filter. (com.jhlabs.image.MinimumFilter.)))

(defn runProgram []
  (show (flood-fill-image ((dilate) (threshold plankton-img))) :zoom 5.0))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (runProgram)
  )
;(def run (runProgram))

;(def plak-pix (get-pixels plankton-img))

;(def test-average (average-pixel-value plak-pix))


