(ns super-koalio.entities
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [super-koalio.utils :as u]))

(defn go-right
  [e]
  (u/get-x-velocity e :right))

(defn jump-ifblocked 
  [{:keys [x-velocity] :as e}]
  (u/get-y-velocity e (if (= x-velocity 0) :jump nil)))

(defn create
  [[x y] [jump-key left-key right-key] [walk-ai jump-ai] stand jump & walk]
  (assoc stand
         :stand-right stand
         :stand-left (texture stand :flip true false)
         :jump-right jump
         :jump-left (texture jump :flip true false)
         :walk-right (animation u/duration
                                walk
                                :set-play-mode (play-mode :loop-pingpong))
         :walk-left (animation u/duration
                               (map #(texture % :flip true false) walk)
                               :set-play-mode (play-mode :loop-pingpong))
         :width 1
         :height (/ 26 18)
         :x-velocity 0
         :y-velocity 0
         :x x
         :y y
         :get-x walk-ai
         :get-y jump-ai
         :decelerate u/decelerate
         :jump-key jump-key
         :left-key left-key
         :right-key right-key
         :me? true
         :can-jump? false
         :direction :right
         :player-score 0))
  

(defn move
  [{:keys [delta-time]} {:keys [x y can-jump? get-x get-y decelerate] :as entity}]
  (let [x-velocity (get-x entity)
        y-velocity (+ (get-y entity) u/gravity)
        x-change (* x-velocity delta-time)
        y-change (* y-velocity delta-time)]
    (if (or (not= 0 x-change) (not= 0 y-change))
      (assoc entity
             :x-velocity (decelerate x-velocity)
             :y-velocity (decelerate y-velocity)
             :x-change x-change
             :y-change y-change
             :x (+ x x-change)
             :y (+ y y-change)
             :can-jump? (if (> y-velocity 0) false can-jump?))
      entity)))

(defn animate
  [screen {:keys [x-velocity y-velocity
                  stand-right stand-left
                  jump-right jump-left
                  walk-right walk-left] :as entity}]
  (let [direction (u/get-direction entity)]
    (merge entity
           (cond
             (not= y-velocity 0)
             (if (= direction :right) jump-right jump-left)
             (not= x-velocity 0)
             (if (= direction :right)
               (animation->texture screen walk-right)
               (animation->texture screen walk-left))
             :else
             (if (= direction :right) stand-right stand-left))
           {:direction direction})))

(defn prevent-move
  [screen {:keys [x y x-change y-change player-score] :as entity}]
  (let [old-x (- x x-change)
        old-y (- y y-change)
        entity-x (assoc entity :y old-y)
        entity-y (assoc entity :x old-x)
        up? (> y-change 0)]
    (merge entity
           (when (u/get-touching-tile screen entity-x "walls")
             {:x-velocity 0 
              :x-change 0 
              :x old-x})
           (when-let [tile (u/get-touching-tile screen entity-y "walls")]
             {:y-velocity 0 
              :y-change 0 
              :y old-y
              :can-jump? (not up?) 
              :to-destroy (when up? [tile "walls"])})
           (when-let [tile (u/get-touching-tile screen entity-y "coins")]
             { :player-score (inc player-score)
               :to-destroy [tile "coins"]}))))
