(ns app.benchmark
  (:require [org.httpkit.client :as httpkit]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [app.core :as app]))

(def benchmark-room-name "benchmark")

(defn clear-data []
  (->> "./data"
       io/file
       file-seq
       (drop 1)
       (map io/delete-file)
       doall))

(def auth-headers
  {"Authorization" "Basic secret"})

(defn sync-room [room & [metadata]]
  @(httpkit/post (str "http://localhost:8080/" room)
                 {:headers auth-headers
                  :body (json/generate-string {:action "syncRoom" :data (into {:users {:test-client {:viewed 0 :typing false}}} metadata)})}))

(defn insert [room message & [author-extra]]
  @(httpkit/post (str "http://localhost:8080/" room)
                 {:headers auth-headers
                  :body (json/generate-string {:action "createMessage" :data (assoc message :author (merge {:id "test-client"} author-extra))})}))

(defn delete [room message-id]
  @(httpkit/post (str "http://localhost:8080/" room)
                 {:headers auth-headers
                  :body (json/generate-string {:action "deleteMessage" :data {:delete-index message-id :author {:id "test-client"}}})}))

(defn read [room & [params userId]]
  @(httpkit/post "http://localhost:8080/"
                 {:headers auth-headers
                  :body (json/generate-string
                         {:userId (or userId "test-client")
                          :chats [(into {:id room} params)]})}))
(defn insert-n [n]
  (doall
   (for [i (range 0 n)]
     (insert benchmark-room-name {:text (str "hello " i)})))
  nil)

(defn read-n [n & [offset viewed]]
  (doall
   (for [i (range 0 n)]
     (read benchmark-room-name (merge
                                {}
                                (when offset {:offset offset})
                                (when viewed {:viewed n}))))) nil)

(defn history-n [n history]
  (doall
   (for [i (range 0 n)]
     (read benchmark-room-name {:history history})))
  nil)

(defn author-anonymize []
  @(httpkit/post (str "http://localhost:8080/anonymizeAuthor")
                 {:headers auth-headers
                  :body (json/generate-string {:author-id "test-client"})}))

(comment
  (do
    (println "STARTING ==================")
    (do
      (clear-data)
      (app/restart)
      (create-room benchmark-room-name)
      nil)
    (do (println "WRITE ==================")
        (time (insert-n 10000))
        (time (insert-n 10000))
        (time (insert-n 10000))
        (time (insert-n 10000))
        (time (insert-n 10000))
        (time (insert-n 10000))
        (time (insert-n 10000))
        (time (insert-n 10000))
        (time (insert-n 10000))
        (time (insert-n 10000))
        (time (insert-n 95)))
    (do (println "READ initial ==================")
        (time (read-n 10000))
        (time (read-n 10000))
        (time (read-n 10000))
        (time (read-n 10000))
        (time (read-n 10000))
        (time (read-n 10000))
        (time (read-n 10000))
        (time (read-n 10000))
        (time (read-n 10000))
        (time (read-n 10000)))
    (do (println "READ OFFSET ==================")
        (time (read-n 10000 99999))
        (time (read-n 10000 99999))
        (time (read-n 10000 99999))
        (time (read-n 10000 99999))
        (time (read-n 10000 99999))
        (time (read-n 10000 99999))
        (time (read-n 10000 99999))
        (time (read-n 10000 99999))
        (time (read-n 10000 99999))
        (time (read-n 10000 99999)))
    (do (println "READ VIEWED ==================")
        (time (read-n 10000 99999 true))
        (time (read-n 10000 99999 true))
        (time (read-n 10000 99999 true))
        (time (read-n 10000 99999 true))
        (time (read-n 10000 99999 true))
        (time (read-n 10000 99999 true))
        (time (read-n 10000 99999 true))
        (time (read-n 10000 99999 true))
        (time (read-n 10000 99999 true))
        (time (read-n 10000 99999 true)))
    (do (println "READ HISTORY ==================")
        (time (history-n 10000 99800))
        (time (history-n 10000 99800))
        (time (history-n 10000 99800))
        (time (history-n 10000 99800))
        (time (history-n 10000 99800))
        (time (history-n 10000 99800))
        (time (history-n 10000 99800))
        (time (history-n 10000 99800))
        (time (history-n 10000 99800))
        (time (history-n 10000 99800)))
    (println "DONE ==================")))
