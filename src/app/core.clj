(ns app.core
  (:require [app.web]
            [org.httpkit.server :as httpkit]
            [app.cache :as cache]))

(set! *warn-on-reflection* true)

(defonce server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn start-server []
  (reset! server (httpkit/run-server #'app.web/app {:port 8080})))

(defn -main[]
  (start-server))

(defn restart []
  (stop-server)
  (reset! cache/topics {})
  (start-server)
  )

(comment
  BATCH Request
  {:userId "1q2w3e4r"
   :chats [{:id "room-1"
            :offset 100
            :viewed 98
            :typing false}
           {:id "room-5"
            :offset 150
            :viewed 140
            :typing false}
           {:id "room-15"
            :offset 15
            :viewed 15
            :typing true}
           ]}
  BATCH Response
  [{:id "room-1"
    :messages [...]
    :users [{:userid "1q2w3e4r"
             :online true}
            {:userid "test"
             :online false}]}
   {:id "room-5"
    :messages [...]
    :users [{:userid "1q2w3e4r"
             :typing false
             :online true}
            {:userid "test"
             :typing false
             :online false}]}
   {:id "room-15"
    :messages [...]
    :users [{:userid "1q2w3e4r"
             :typing true
             :online true}
            {:userid "test"
             :typing false
             :online false}]}]
  )

(comment
 (-main)
 (restart)
  
 (load-index-cache (io/file "./data/foo.data") (io/file "./data/foo.index"))

 (let [reader (io/input-stream (io/file "./data/count.data"))
       data (byte-array 10)]
   (.read reader data)
   data
   )

 (get-in @topics ["foo" :index-cache :line-index])

 )
