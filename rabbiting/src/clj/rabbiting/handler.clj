(ns rabbiting.handler
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [compojure.core :refer [GET defroutes]]
            [ring.util.response :refer [resource-response response]]
            [ring.middleware.json :as middleware]
            [org.httpkit.server :refer :all]
            )
  )

(def clients (atom {}))

(defn ws
  [req]
  (println req)
  (with-channel req con
    (swap! clients assoc con true)
    (println con " connected")
    (on-receive con (fn [& args]
    	     	    (println args)
		    (send! con (str "Roundtripped " args))))
    (on-close con (fn [status]
                    (swap! clients dissoc con)
                    (println con " disconnected. status: " status)))))

(defroutes app-routes
  (GET  "/" [] (resource-response "index.html" {:root "public"}))
  (GET  "/widgets" [] (response [{:name "Widget 1"} {:name "Widget 2"}]))
  (GET "/chatwebsocket" []  ws)
  (route/resources "/")
  (route/not-found "Page not found"))

(def app
  (-> (handler/api app-routes)
      (middleware/wrap-json-body)
      (middleware/wrap-json-response)))
