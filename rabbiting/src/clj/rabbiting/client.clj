(ns rabbiting.client
    (:use rabbiting.handler)
    (:require  [rabbiting.core :as c]
	       [compojure.handler :only [site]] )
(:use [org.httpkit.server :only [run-server]])	
  (:gen-class))

(defn -main [& args]
      (org.httpkit.server/run-server (compojure.handler/site rabbiting.handler/app-routes) {:port 8080}))

