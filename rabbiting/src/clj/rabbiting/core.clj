(ns rabbiting.core
  (:refer-clojure :exclude [send])
  (:require [langohr.core      :as rmq]
            [langohr.channel   :as lch]
            [langohr.queue     :as lq]
            [langohr.consumers :as lc]
            [langohr.basic     :as lb]))

; http://clojurerabbitmq.info/articles/getting_started.html

(def ^{:const true}
  all-incoming-exchange "all.incoming")

(def ^{:const true} routing-key "rabbiting.message.sent")

(def connected-users (atom #{}))

(defn message-handler
  [client-msg-handler ch {:keys [content-type delivery-tag type sender headers] :as meta} ^bytes payload]
  (let [message (String. payload "UTF-8")]
    (when (= type "user.joined")
      (swap! connected-users #(conj % message)))
    (println meta)
    (client-msg-handler (get headers "sender") message)))

(defn shutdown [{:keys [connection channel]}]
  (println "[main] Disconnecting...")
  (rmq/close channel)
  (rmq/close connection))

(defn queue-name-for-user [user]
  (str "rabbiting.user." user))

(defn initialize-connection
  [client-msg-handler user]
  (let [conn  (rmq/connect)
        ch    (lch/open conn)
        qname    (queue-name-for-user user)]
    (println (format "[main] Connected. Channel id: %d" (.getChannelNumber ch)))
    (lq/declare ch qname :exclusive false :auto-delete true)
    (lq/bind ch qname all-incoming-exchange :routing-key routing-key)
    (lc/subscribe ch qname (partial message-handler client-msg-handler) :auto-ack true )
    {:connection conn :channel ch :user user}))

(defn send [msg-type msg-body {:keys [channel user]}]
    (lb/publish channel
                all-incoming-exchange
                routing-key
                msg-body
                :content-type "text/plain"
                :type msg-type
                :headers {"sender" user}))

(defn join [connection-info]
  (send "user.joined" (:user connection-info) connection-info))

(defn message! [body connection-info]
  (send "user.msg.sent" body connection-info))
