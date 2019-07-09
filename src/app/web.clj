(ns app.web
  (:require [clojure.string :as str]
            [app.persist :as persist]
            [app.cache :as cache]
            [org.httpkit.server :as httpkit]
            [clojure.java.io :as io]
            [clj-time.core :as time]
            [cheshire.core :as json])
  (:import java.io.File
           java.io.InputStream
           java.io.Writer))

(defn parse-quesrystring [qs]
  (if qs
    (->> (str/split qs #"&")
         (map #(str/split % #"="))
         (map (fn [[k v]] [(keyword k) v]))
         (into {}))
    {}))

(defn index []
  {:status 200
   :body (slurp "./resources/html/index.html")}
  )

(defn app [req]
   (let [{uri :uri action :request-method} req
        filename (subs uri 1)
        file-config (cache/get-file-config filename)]
    (case uri
      "/" (index)
      (httpkit/with-channel req channel
        (let [file (:file file-config)
              params (parse-quesrystring (:query-string req))]
          (locking file
            (if (= action :post)
              (let [^java.io.Writer writer (:writer file-config)
                    ^java.io.Writer index-writer (:index-writer file-config)
                    index-file (:index-file file-config)
                    data (json/parse-string (slurp (:body req)))
                    data (json/generate-string
                          (assoc data
                                 :message-index (+ 1 (get-in @cache/topics [filename :index-cache :lines-count]))
                                 :timestamp (str (time/now))))
                    new-data-length (+ (count data) 1)]
                (cache/update-cache-index filename (fn [{:keys [lines-count length line-index last-index]}]
                                               (let [count (+ lines-count 1)
                                                     need-index (= 0 (mod count persist/index-step))]
                                                 {:lines-count count
                                                  :length (+ new-data-length length)
                                                  :last-index (if need-index count last-index)
                                                  :line-index (if need-index
                                                                (do
                                                                  (.write index-writer (str count " " length "\n"))
                                                                  (.flush index-writer)
                                                                  (assoc line-index count length)
                                                                  )
                                                                line-index)})))
                (.write writer data)
                (.write writer "\n")
                (.flush writer)
                (httpkit/send! channel {:status  200
                                        :headers {"Content-Type" "text/html"
                                                  "Access-Control-Allow-Origin" "*"}
                                        :body    ""}))
              (let [reader (:reader file-config)
                    {:keys [offset history]} params
                    offset (when offset (Integer/parseInt offset))
                    history (when history (Integer/parseInt history))]
                (if (not (or (nil? offset) (nil? history)))
                  (httpkit/send! channel {:status  422
                                          :headers {"Content-Type" "text/html"}})
                  (httpkit/send! channel {:status  200
                                          :headers {"Content-Type" "text/html"
                                                    "Access-Control-Allow-Origin" "*"}
                                          :body (persist/read-stream file-config offset history)}))))
            (httpkit/close channel)))))))
