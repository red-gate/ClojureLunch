(ns rabbiting.client
  (:require  [rabbiting.core :as c])
  (:gen-class))

(defn -main [& args]
  (let [user (read-line)
        connection-info (c/initialize-connection user)]
    (c/shutdown connection-info))
  )
