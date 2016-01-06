(ns super-koalio.utils
  (:require [play-clj.core :refer :all]
            [play-clj.utils :as u]))

(def ^:const vertical-tiles 20)
(def ^:const pixels-per-tile 16)
(def ^:const duration 0.15)
(def ^:const damping 0.5)
(def ^:const max-velocity 14)
(def ^:const max-jump-velocity (* max-velocity 4))
(def ^:const deceleration 0.9)
(def ^:const gravity -2.5)

(defn decelerate
  [velocity]
  (let [velocity (* velocity deceleration)]
    (if (< (Math/abs velocity) damping)
      0
      velocity)))

(defn touched?
  [key]
  (and (game :touched?)
       (case key
         :down (< (game :y) (/ (game :height) 3))
         :up (> (game :y) (* (game :height) (/ 2 3)))
         :left (< (game :x) (/ (game :width) 3))
         :right (> (game :x) (* (game :width) (/ 2 3)))
         :center (and (< (/ (game :width) 3) (game :x) (* (game :width) (/ 2 3)))
                      (< (/ (game :height) 3) (game :y) (* (game :height) (/ 2 3))))
         false)))

(defn fixed-key-code
  [k]
  (.get 
   (first (filter (fn [x] (= (.getName x) (u/key->upper k)))
                  (.getDeclaredFields com.badlogic.gdx.Input$Keys))) nil))

(defn fixed-key-pressed?
  [k]
  (input! :is-key-pressed (fixed-key-code k))
  )

(defn get-x-velocity
  [{:keys [me? x-velocity]} direction]
  (if me?
    (cond
      (= direction :left)
      (* -1 max-velocity)
      (= direction :right)
      max-velocity
      :else
      x-velocity)
    x-velocity))

(defn get-x-velocity-from-keys [ {:keys [me? x-velocity left-key right-key] :as obj}] 
  (get-x-velocity obj 
                  (cond 
                    (or (fixed-key-pressed? left-key) (touched? :left))
                    :left
                    (or (fixed-key-pressed? right-key) (touched? :right))
                    :right
                    :else
                    nil
                    )))


(defn get-y-velocity
  [{:keys [me? y-velocity can-jump?]} jump-pressed]
  (if me?
    (cond
      (and can-jump? jump-pressed)
      max-jump-velocity
      :else
      y-velocity)
    y-velocity))

(defn get-y-velocity-from-keys
  [{:keys [jump-key] :as e}]
  (get-y-velocity e (or (fixed-key-pressed? jump-key) (touched? :up)))
)

(defn get-direction
  [{:keys [x-velocity direction]}]
  (cond
    (> x-velocity 0) :right
    (< x-velocity 0) :left
    :else
    direction))

(defn get-tiles-in
  [screen layer-names heights widths]
  (let [layers (map #(tiled-map-layer screen %) layer-names)]
       (for [tile-x widths
             tile-y heights]
         (some #(when (tiled-map-cell % tile-x tile-y)
                  [tile-x tile-y])
               layers))))

(defn get-tiles-on-screen
  [screen & layer-names]
  (let [x (x screen)
        y (y screen)
        width (/ (width screen) 2)]
    (when-let [height (and (height screen) (/ (height screen) 2))]
      (when-let [widths (range (int (- x width)) (+ x width))] 
        (when-let [heights (range (int (- y height)) (+ y height))] 
          (drop-while nil? (get-tiles-in screen layer-names heights widths))))))
  )
 
(defn get-touching-tile
  [screen {:keys [x y width height]} & layer-names]
  (->> 
   (let [widths (range (int x) (+ x width))
         heights (range (int y) (+ y height))] 
     (get-tiles-in screen layer-names heights widths))
   
   first))
