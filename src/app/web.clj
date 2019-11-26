(ns app.web
  (:require [clojure.string :as str]
            [clojure.set :as clojure_set]
            [app.cache :as cache]
            [org.httpkit.client :as httpkit-client]
            [org.httpkit.server :as httpkit]
            [cheshire.core :as json]
            [cheshire.generate :as json-proto]))

(extend-protocol json-proto/JSONable
  org.joda.time.DateTime
  (to-json [dt gen]
    (json-proto/write-string gen (str dt))))

(defn parse-quesrystring [qs]
  (if qs
    (->> (str/split qs #"&")
         (map #(str/split % #"="))
         (map (fn [[k v]] [(keyword k) v]))
         (into {}))
    {}))

(def response-headers {"Access-Control-Allow-Headers" "*"
                       "Access-Control-Allow-Methods" "*"
                       "Access-Control-Allow-Origin" "*"
                       "Access-Control-Allow-Credentials" "*"
                       "Access-Control-Expose-Headers" "*"
                       "Content-Type" "application/json"
              })

(defn index []
  {:status 200
   :headers (assoc response-headers "Content-Type" "text/html")
   :body (slurp "./resources/html/index.html")})

(defn batch-operation [{:keys [userId chats]}]
  {:status 200
   :headers response-headers
   :body (json/generate-string
          (map (fn [{id :id :as meta}]
                 (assoc (cache/read-messages id (assoc meta :userId (keyword userId)))
                        :id id))
               chats))})

(defonce auth (atom {}))

(def chat-secret (get (System/getenv) "CHAT_SECRET"))
(def chat-auth-remote (get (System/getenv) "CHAT_AUTH_REMOTE"))

(defn extract-chats [req]
  (let [{uri :uri action :request-method headers :headers} req]
    (case uri
      "/" (if (= action :post)
            (let [data (:body req)]
              (set (map :id (:chats data))))
              (set []))
      (let [{:keys [action data]} (:body req)
            [_ filename](str/split uri #"/")]
        (case action
          "createMessage" (set [filename])
          "deleteMessage" (set [filename])
          false)))))


(defn check-auth [authorization req]
  (if-let [auth-data (get @auth authorization)]
    (let [chats (extract-chats req)]
      (if (clojure_set/subset? chats auth-data)
        true
        (let [chats (extract-chats req)
              body {:chats chats}
              response @(httpkit-client/post
                         chat-auth-remote
                         {:headers {"authorization" authorization
                                    "content-type" "application/json"}
                          :method :post
                          :body (json/generate-string body)})]
          (if (= (:status response) 200)
            (swap! auth assoc authorization chats)
            false))))
    (when (= (:uri req) "/")
      (let [chats (extract-chats req)
            body {:chats chats}
            response @(httpkit-client/post
                       chat-auth-remote
                       {:headers {"authorization" authorization
                                  "content-type" "application/json"}
                        :method :post
                        :body (json/generate-string body)})]
        (if (= (:status response) 200)
          (swap! auth assoc authorization chats)
          false)))))

(defn is-authorized [req]
  (let [authorization (get (:headers req) "authorization")]
    (or
     (= chat-secret authorization)
     (check-auth authorization req))))

(defn app [req]
  (let [{uri :uri action :request-method headers :headers} req]
    (if (= action :options)
      {:status 200
       :headers response-headers
       :body {}}
      (if (and (= uri "/")
               (= action :get))
        (index)
        (let[req (assoc req :body (json/parse-string (slurp (:body req)) keyword))
             authorization (get (:headers req) "authorization")]
          (if (is-authorized req)
            (case uri
              "/" (batch-operation (:body req))
              (let [params (parse-quesrystring (:query-string req))]
                (if (= action :post)
                  (let [{:keys [action data]} (:body req)
                        [_ filename](str/split uri #"/")]
                    (case action
                      "deleteMessage" (do
                                        (cache/delete-message filename data authorization)
                                        {:status 200
                                         :headers response-headers
                                         :body {}})
                      "createMessage" (do
                                        (cache/write-message filename data authorization)
                                        {:status 200
                                         :headers response-headers
                                         :body {}})
                      "createRoom" (do
                                     (cache/create-room filename data)
                                     {:status 201
                                      :headers response-headers
                                      :body {}})
                      "updateRoom" (do
                                     (cache/update-room filename data)
                                     {:status 200
                                      :headers response-headers
                                      :body {}})
                      {:status 422
                       :headers response-headers}))
                  {:status 422
                   :headers response-headers})))
            {:status 403
             :headers response-headers}))))))

(comment
  (reset! auth {})

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
