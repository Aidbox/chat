(ns app.web
  (:require [clojure.string :as str]
            [app.cache :as cache]
            [org.httpkit.server :as httpkit]
            [cheshire.core :as json]))

(defn parse-quesrystring [qs]
  (if qs
    (->> (str/split qs #"&")
         (map #(str/split % #"="))
         (map (fn [[k v]] [(keyword k) v]))
         (into {}))
    {}))

(defn index []
  {:status 200
   :body (slurp "./resources/html/index.html")})

(defn app [req]
  (let [{uri :uri action :request-method} req
        filename (subs uri 1)]
    (case uri
      "/" (index)
      (httpkit/with-channel req channel
        (let [params (parse-quesrystring (:query-string req))]
          (if (= action :post)
            (do
              (cache/write-message filename (json/parse-string (slurp (:body req))))
              (httpkit/send! channel {:status 200
                                      :headers {"Content-Type" "text/html"
                                                "Access-Control-Allow-Origin" "*"}
                                      :body ""}))
            (let [{:keys [offset history]} params
                  offset (when offset (Integer/parseInt offset))
                  history (when history (Integer/parseInt history))]
              (if (not (or (nil? offset) (nil? history)))
                (httpkit/send! channel {:status 422
                                        :headers {"Content-Type" "text/html"}})
                (httpkit/send! channel {:status 200
                                        :headers {"Content-Type" "text/html"
                                                  "Access-Control-Allow-Origin" "*"}
                                        :body (cache/read-messages filename offset history)}))))
          (httpkit/close channel))))))
