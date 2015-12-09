(ns super-koalio.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.ui :refer :all]
            [super-koalio.entities :as e]
            [super-koalio.utils :as u]))

(declare super-koalio main-screen text-screen)

(defn update-screen!
  [screen entities]
  (doseq [{:keys [x y height me? to-destroy]} entities]
    (when me?
      (position! screen x y)
      (when (< y (- height))
        (set-screen! super-koalio main-screen text-screen)))
    (when-let [[[tile-x tile-y] layer] to-destroy]
      (tiled-map-layer! (tiled-map-layer screen layer)
                        :set-cell tile-x tile-y nil)))
  (map #(dissoc % :to-destroy) entities))

(def global-entities (atom nil))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (->> (orthogonal-tiled-map "level1.tmx" (/ 1 u/pixels-per-tile))
         (update! screen :timeline [] :camera (orthographic) :renderer))
    (let [sheet (texture "koalio.png")
          tiles (texture! sheet :split 18 26)
          player-images (for [col [0 1 2 3 4]]
                          (texture (aget tiles 0 col)))]
      [(apply e/create [10 10] [:dpad-up :dpad-left :dpad-right] [e/ai-set-direction e/jump-ifblocked] player-images)
]))
  
  :on-render
  (fn [screen entities]
    (clear! 0.5 0.5 1 1)
    (reset! global-entities (u/get-tiles-on-screen screen "walls"))
    (screen! text-screen :on-score :something-crazy (reduce #(+ %1 (or (:player-score %2) 0 )) 0 entities))
    (some->> (if (or (key-pressed? :space) (u/touched? :center))
               (rewind! screen 2)
               (map (fn [entity]
                      (->> entity
                           (#(assoc % :ai-direction :right))
                           (e/move screen)
                           (e/prevent-move screen)
                           (e/animate screen)))
                    entities))
             (render! screen)
             (update-screen! screen)))
  
  :on-resize
  (fn [{:keys [width height] :as screen} entities]
    (width! screen 20)))

(defscreen text-screen
  :on-show
  (fn [screen entities]
    (update! screen :camera (orthographic) :renderer (stage))
     [(assoc (label "0" (color :white))
             :id :fps
             :x 5) 
      (assoc (label "0" (color :green))
             :id :score
             :x 150)])
  
  :on-score
  (fn [screen entities]
   (for [entity entities]
           (case (:id entity)
             :score (doto entity 
                      (label! :set-text 
                              (str "score: " (:something-crazy screen))))
             entity
           )))
  
  :on-render
  (fn [screen entities]
    
    (->> (for [entity entities]
           (case (:id entity)
             :fps (doto entity (label! :set-text (str (game :fps))))
             entity
           ))
         (render! screen)))
  
  :on-resize
  (fn [screen entities]
    (height! screen 300)))

(defgame super-koalio
  :on-create
  (fn [this]
    (set-screen! this main-screen text-screen)))

