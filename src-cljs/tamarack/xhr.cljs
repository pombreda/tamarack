(ns tamarack.xhr
  (:require [goog.events :as events]
            [cognitect.transit :as transit])
  (:import [goog.net XhrIo]
           goog.net.EventType
           [goog.events EventType]))

(def ^:private meths
  {:get "GET"
   :put "PUT"
   :post "POST"
   :delete "DELETE"})

(defn send-edn [{:keys [method url data on-complete]}]
  (let [xhr (XhrIo.)
        reader (transit/reader :json)]
    (events/listen xhr goog.net.EventType.COMPLETE
      (fn [e]
        (on-complete (transit/read reader (.getResponseText xhr)))))
    (. xhr
      (send url (meths method) (when data (pr-str data))
        #js {"Accept" "application/transit+json"}))))
