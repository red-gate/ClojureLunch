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

(defn send-chat-message!
  [event]
  (when (= (.-keyCode event) 13)
    (let [entered-value (-> event .-currentTarget .-value)]
      (.send socket entered-value))
    (set! (-> event .-currentTarget .-value) "")))

(defn connect-as-user! [username])

(defn msg-prompt []
  [:input {:type "text" :on-key-up send-chat-message!}])

(defn username-prompt []
  [:input {:type "text"}])

(defn submit-username []
  [:button {:on-click (connect-as-user! "Toby")}
   "Be Toby"])

(defn ^:export run []
  (reagent/render-component [:div
                             [submit-username]
                             [msg-list]
                             [msg-prompt]]
                            (.-body js/document)))



