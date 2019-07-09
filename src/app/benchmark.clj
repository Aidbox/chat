(ns app.benchmark
  (:require [org.httpkit.client :as httpkit]
            [clojure.java.io :as io]
            [app.core :as app]))

(defn insert-n [n]
  (doall
   (for [i (range 0 n)]
     (let [resp @(httpkit/post "http://localhost:8080/foo" {:body (str "{\"text\": \"hello " i "\"}")}) ]
       nil)))
  nil)

(defn read-n [n & [offset]]
  (doall
   (for [i (range 0 n)]
     (let [resp @(httpkit/get "http://localhost:8080/foo" (when offset {:query-params {:offset offset}}))]
       nil)))
  nil)

(defn history-n [n history]
  (doall
   (for [i (range 0 n)]
     (let [resp @(httpkit/get "http://localhost:8080/foo" {:query-params {:history history}})]
       nil)))
  nil)



(comment

  (do
    (do
      (try
        (io/delete-file (str "./data/foo.data"))
        (catch Exception e nil))
      (try
        (io/delete-file (str "./data/foo.index"))
        (catch Exception e nil))
      (app/restart))
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
    )

  )
