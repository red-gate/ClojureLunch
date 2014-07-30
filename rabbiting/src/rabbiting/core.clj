(ns rabbiting.core
  (:gen-class)
  (:require [langohr.core      :as rmq]
            [langohr.channel   :as lch]
            [langohr.queue     :as lq]
            [langohr.consumers :as lc]
            [langohr.basic     :as lb]))

; http://clojurerabbitmq.info/articles/getting_started.html

(def ^{:const true}
  all-incoming-exchange "all.incoming")

(def connected-users (atom #{}))

(defn message-handler
  [ch {:keys [content-type delivery-tag type] :as meta} ^bytes payload]
  (let [message (String. payload "UTF-8")]
    (when (= type "user.joined")
      (swap! connected-users #(conj % message)))
    (println (format "[consumer] Received a message: %s, delivery tag: %d, content type: %s, type: %s"
                      message delivery-tag content-type type))))

(defn -main
  [& args]
  (let [user "arun"
        conn  (rmq/connect)
        ch    (lch/open conn)
        qname (str "rabbiting.user." user)
        routing-key "rabbiting.message.sent"
        ]
    (println (format "[main] Connected. Channel id: %d" (.getChannelNumber ch)))
    (lq/declare ch qname :exclusive false :auto-delete true)
    (lq/bind ch qname all-incoming-exchange :routing-key routing-key)
    (lc/subscribe ch qname message-handler :auto-ack true)
    (lb/publish ch all-incoming-exchange routing-key user :content-type "text/plain" :type "user.joined")
    (println "[main] Disconnecting...")
    (rmq/close ch)
    (rmq/close conn)))

(-main)

@connected-users
