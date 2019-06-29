(ns app.benchmark
  (:require [org.httpkit.client :as httpkit]))

(defn insert-n [n]
  (doall
   (for [i (range 0 n)]
     (let [resp @(httpkit/post "http://localhost:8080/foo" {:body (str "{\"message\": \"hello " i "\"}")}) ]
       nil)))
  nil)

(defn read-n [n & [offset]]
  (doall
   (for [i (range 0 n)]
     (let [resp @(httpkit/get "http://localhost:8080/foo" {:query-params {:offset (or offset 0)}})]
       nil)))
  nil)


(comment

  (time (insert-n 6000))

  (time (read-n 1500 0))


  )
