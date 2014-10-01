(ns rabbiting.core
  (:require [reagent.core :as reagent :refer [atom]]))

(def msg-list-atom (atom ["123", "456"]))

(defn msg-list []
  [:div
   (for [msg @msg-list-atom]
     [:p msg])])

(defn asdf [event]
  (when (= (.-keyCode event) 13)
    (swap! msg-list-atom conj (-> event .-currentTarget .-value))
    (set! (-> event .-currentTarget .-value) "")))

(defn msg-prompt []
  [:input {:type "text" :on-key-up asdf}])

(defn ^:export run []
  (reagent/render-component [:div [msg-list] [msg-prompt]]
                            (.-body js/document))) 
