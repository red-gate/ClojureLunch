(ns rabbiting.client
  (:gen-class)
  (:require  [rabbiting.core :as c]))

(def connection-info
  (c/initialize-connection "arun"))

