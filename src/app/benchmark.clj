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
     (let [resp @(httpkit/get "http://localhost:8080/foo" {:query-params {:offset (or offset 0)}})]
       nil)))
  nil)


(comment

  (do
    (io/delete-file "./data/foo.data")
    (io/delete-file "./data/foo.index")
    (app/restart)
    (println "WRITE ==================")
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
    (println "READ ==================")
    (time (read-n 10000 99999))
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

  )
