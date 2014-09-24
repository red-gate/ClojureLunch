(ns rabbiting.core
  (:require [reagent.core :as reagent :refer [atom]]))

(def click-count (atom 0))

(defn msg-list []
  [:div
   [:h1 @click-count]])

(defn asdf [event]
  (when (= (.-keyCode event) 13)
    (swap! click-count inc)))

(defn msg-prompt []
  [:input {:type "text" :on-key-up asdf}])

(defn ^:export run []
  (reagent/render-component [:div [msg-list] [msg-prompt]]
                            (.-body js/document))) 
