(ns rabbiting.server
    (:require
     [rabbiting.core :as rc]
     [org.httpkit.server :refer :all]
	    ))

(def clients (atom {}))

(defn push-chat-message [con args] (send! con (str "Roundtripped " args)))

(defn websocket
  [req]
  (println req)
  (with-channel req con
		(swap! clients assoc con true)
		(println con " connected")
		
		(let [chat-con (rc/initialize-connection "Toby")]
		
		     (on-receive con (fn [& args]
					 (println args)
					 (push-chat-message con args)))
		     (on-close con (fn [status]
				       (swap! clients dissoc con)
				       (println con " disconnected. status: " status))))))

