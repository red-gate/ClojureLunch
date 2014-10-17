(ns rabbiting.core
  (:require [reagent.core :as reagent :refer [atom]]))

(def msg-list-atom (atom ["123", "456"]))

(defn msg-list []
  [:div
   (for [msg @msg-list-atom]
     [:p msg])])

(def socket (js/WebSocket. "ws://localhost:8080/chatwebsocket"))

(set! (.-onmessage socket)
     (fn [event]
        (let [data (-> event .-data)]
          (swap! msg-list-atom conj data))))
   
(defn asdf [event]
  (when (= (.-keyCode event) 13)
    (let [entered-value (-> event .-currentTarget .-value)]
      (.send socket entered-value))
    (set! (-> event .-currentTarget .-value) "")))

(defn msg-prompt []
  [:input {:type "text" :on-key-up asdf}])

(defn ^:export run []
  (reagent/render-component [:div [msg-list] [msg-prompt]]
                            (.-body js/document))) 



