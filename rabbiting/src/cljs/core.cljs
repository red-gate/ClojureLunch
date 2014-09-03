(ns rabbiting-om.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state (atom {:text "Hello Tilman!"
                      :messages [{:text "First message"} {:text "Second message"}]}))

(defn send-message [app owner]
  (let [message-input (-> (om/get-node owner "message-input")
                          .-value)]
    (.log js/console message-input)))

(defn input-view [app owner]
  (reify
    om/IRender
    (render [this]
      (dom/div nil
        (dom/input #js {:type "text" :ref "message-input"})
        (dom/button #js {:onClick #(send-message app owner)} "Send!")))))

(defn message-view [message owner]
  (reify
    om/IRender
    (render [this]
      (dom/li nil (:text message)))))

(defn messages-view [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        (dom/h3 nil "Messages go here")
          (apply dom/ul nil
            (om/build-all message-view (:messages app)))))))


(om/root
  (fn [app owner]
    (reify om/IRender
      (render [_]
        (dom/h1 nil (:text app)))))
  app-state
  {:target (. js/document (getElementById "app"))})



(om/root input-view app-state
  {:target (. js/document (getElementById "inputArea"))})


(om/root messages-view app-state
  {:target (. js/document (getElementById "messages"))})
