(ns app.benchmark
  (:require [org.httpkit.client :as httpkit]))

(defn insert-n [n]
  (doall
   (for [i (range 0 n)]
     (let [resp @(httpkit/post "http://localhost:8080/foo" {:body (str "{\"message\": \"hello " i "\"}")}) ]
       nil))))

(defn read-n [n]
  (doall
   (for [i (range 0 n)]
     (let [resp @(httpkit/get "http://localhost:8080/foo")]
       nil))))


(comment

  (time (read-n 1000))

  (time (insert-n 1000))

  )
