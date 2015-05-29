(ns plankton.core
  (:gen-class)
  (:import [java.awt.image BufferedImage BufferedImageOp])
  (:require [clojure.tools.nrepl.server :as nrepl-server]
            [cider.nrepl :refer (cider-nrepl-handler)])
  (:use plankton.imageprocessing))

(use 'mikera.image.core)

(use 'mikera.image.colours)

(def image-path "test-image.jpg")

(def plankton-img (load-image image-path))

(def pixels (get-pixels plankton-img))

(defn byte-mangling [b] (int (bit-and b 0xFF)))

(defn seq-map-byte-array [f b]
  (byte-array (f (seq b))))

(defn pixelfilter [f image]
  (let [pixels (get-pixels image)]
    (set-pixels image (seq-map-byte-array (comp f #(map byte-mangling %)) pixels))
    image
))

(defn threshold [image]
  (threshold-pixels image))

;; (defn neighbours [index width length]
;;   (let [valid? #(and (>= % 0) (< % length))]
;;   (filter valid? [(dec index) (inc index) (+ index width) (- index width)])
;;   ))

;; (defn flood-fill-from-index-with-colour [pixs index old-colour new-colour]

;;   (persistent! (loop [pixels-to-check (list index)
;;                       pixels (transient (vec pixs))]
;;                  (if (first pixels-to-check)
;;                    (let [current (first pixels-to-check)

;;                          ]

;;                      (if (= (get pixels current) old-colour)
;;                        (recur (concat (rest pixels-to-check)
;;                                       (neighbours current (.getWidth plankton-img)
;;                                                   (count pixels)))
;;                               (assoc! pixels current new-colour))
;;                        (recur (rest pixels-to-check)  pixels)
;;                        )
;;                      )
;;                     pixels))))


;; (defn flood-fill-pixels [pixels]
;;   (let [enumerated-pixels (zipmap pixels (iterate inc 0))
;;         old-colour 255
;;         index (second (find enumerated-pixels old-colour))]
;;     (println "index")
;;     (println index)
;;     (flood-fill-from-index-with-colour pixels index old-colour 210))
;;   )

;; ;;(flood-fill-pixels [0 255 255 0])


;; (defn flood-fill-image [image]
;;   (pixelfilter flood-fill-pixels image))

;; (deftype Filter [^BufferedImageOp image-op]
;;   clojure.lang.IFn
;;   (invoke [this image]
;;           (let [^BufferedImage image image dest-img (.createCompatibleDestImage image-op image (.getColorModel image))]
;;             (.filter image-op image dest-img) dest-img))
;;   (applyTo [this args] (clojure.lang.AFn/applyToHelper this args)))

;; (defn dilate []
;;   ;  "Creates a simple blur filter (3x3 pixel) blur"
;;   (Filter. (com.jhlabs.image.MinimumFilter.)))

;; (defn runProgram []
;;   (show (flood-fill-image ((dilate) (threshold plankton-img))) :zoom 7.0))

(show (threshold plankton-img) :zoom 7.0)

;(defn -main
;  "I don't do a whole lot ... yet."
;  [& args]
;  (nrepl-server/start-server :port 7888 :handler cider-nrepl-handler))

(def run (runProgram))

(def plak-pix (get-pixels plankton-img))


plak-pix

;(def test-average (average-pixel-value plak-pix))


