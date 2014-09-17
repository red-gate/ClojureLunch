(ns rabbiting.core
  (:require [reagent.core :as reagent :refer [atom]]))

(defn msg-list []
  [:div
   [:h1 "Hello Clojure Lunch"]])

(defn ^:export run []
  (reagent/render-component [msg-list]
                            (.-body js/document))) 
