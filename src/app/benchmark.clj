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

(defn create-room [room & [meatadata]]
  @(httpkit/post (str "http://localhost:8080/" room)
                 {:body (json/generate-string {:action "createRoom" :data (into {} meatadata)})}))

(defn insert [room message]
  @(httpkit/post (str "http://localhost:8080/" room)
                 {:body (json/generate-string {:action "createMessage" :data message})}))

(defn read [room & [params]]
  @(httpkit/post "http://localhost:8080/"
                 {:body (json/generate-string
                         {:userId "test-client"
                          :chats [(into {:id room} params)]})}))
(defn insert-n [n]
  (doall
   (for [i (range 0 n)]
     (insert benchmark-room-name {:text (str "hello " i)})))
  nil)

(defn read-n [n & [offset]]
  (doall
   (for [i (range 0 n)]
     (read benchmark-room-name (when offset {:offset offset}))))
  nil)

(defn history-n [n history]
  (doall
   (for [i (range 0 n)]
     (read benchmark-room-name {:history history})))
  nil)

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
    (println "DONE ==================")
    )

  )
