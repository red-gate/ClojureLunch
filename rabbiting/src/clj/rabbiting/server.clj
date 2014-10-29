(ns rabbiting.server
    (:require
     [rabbiting.core :as rc]
     [org.httpkit.server :refer :all]
	    ))

(def clients (atom {}))

(defn push-chat-message [chat-con args]
  (rc/message! (first args)  chat-con))

(defn recv-chat-message [con user message]
  (send! con (str user ": " message)))

(defn websocket
  [req]
  (println req)
  (with-channel req con
		(swap! clients assoc con true)
		(println con " connected")

		(let [chat-con (rc/initialize-connection (partial recv-chat-message con) "Toby")]

		     (on-receive con (fn [& args]
					 (println args)
					 (push-chat-message chat-con args)))
		     (on-close con (fn [status]
               (rc/shutdown chat-con)
				       (swap! clients dissoc con)
				       (println con " disconnected. status: " status))))))

