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

(defn batch-operation [filename body]
  (let [request (json/parse-string body keyword)]
    {:status 200
     :body (map (fn [{:keys [id offset history]}]
                  {:id id
                   :messages (cache/read-messages id offset history )})
                (:chats request))}))

(defn app [req]
  (let [{uri :uri action :request-method} req
        [_ filename](str/split uri #"/")]
    (case uri
      "/" (if (= action :get)
            (index)
            (batch-operation filename (slurp (:body req))))
      (let [params (parse-quesrystring (:query-string req))]
        (if (= action :post)
          (let [{:keys [action data]} (json/parse-string (slurp (:body req)) keyword)]
            (case action
              "createMessage" (do
                                (cache/write-message filename data)
                                {:status 200
                                 :headers {"Content-Type" "text/html"
                                           "Access-Control-Allow-Origin" "*"}
                                 :body ""})
              "createRoom" (do
                             (cache/create-room filename data)
                             {:status 201
                              :headers {"Content-Type" "text/html"
                                        "Access-Control-Allow-Origin" "*"}
                              :body ""})
              {:status 422
               :headers {"Content-Type" "text/html"}}))
          (let [{:keys [offset history]} params
                offset (when offset (Integer/parseInt offset))
                history (when history (Integer/parseInt history))]
            (if (not (or (nil? offset) (nil? history)))
              {:status 422
               :headers {"Content-Type" "text/html"}}
              {:status 200
               :headers {"Content-Type" "text/html"
                         "Access-Control-Allow-Origin" "*"}
               :body (cache/read-messages filename offset history)})))))))

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
