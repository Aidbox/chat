(ns app.web
  (:require [clojure.string :as str]
            [clojure.set :as clojure_set]
            [app.cache :as cache]
            [org.httpkit.client :as httpkit-client]
            [org.httpkit.server :as httpkit]
            [cheshire.core :as json]
            [cheshire.generate :as json-proto]
            [clj-time.core :as time]))

(extend-protocol json-proto/JSONable
  org.joda.time.DateTime
  (to-json [dt gen]
    (json-proto/write-string gen (str dt))))

(def response-headers {"Access-Control-Allow-Headers" "*"
                       "Access-Control-Allow-Methods" "*"
                       "Access-Control-Allow-Origin" "*"
                       "Access-Control-Allow-Credentials" "*"
                       "Access-Control-Expose-Headers" "*"
                       "Content-Type" "application/json"})

(defn json-resp [status body]
  {:status status
   :headers response-headers
   :body (json/generate-string body)})

(defn index []
  {:status 200
   :headers (assoc response-headers "Content-Type" "text/html")
   :body (slurp "./resources/html/index.html")})

(defn batch-operation [{:keys [userId chats]}]
  (json-resp 200
        (remove nil?
          (map (fn [{id :id :as meta}]
                 (try
                   (assoc
                    (cache/read-messages id (assoc meta :userId (keyword userId)))
                    :id id)
                   (catch Exception e
                     nil)))
               chats))))

(defonce auth (atom {}))

(def chat-secret (get (System/getenv) "CHAT_SECRET"))
(def chat-auth-remote (get (System/getenv) "CHAT_AUTH_REMOTE"))
(def chat-auth-cache-ttl (Integer/parseInt (get (System/getenv) "CHAT_AUTH_CACHE_TTL" "1")))

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
  (let [chats (extract-chats req)
        {auth-expires :expires
         auth-chats :chats} (get @auth authorization
                                 {:expires (time/now) :chats (set [])})]
    (if (and
         (time/after? auth-expires (time/now))
         (clojure_set/subset? chats auth-chats))
      true
      (let [body {:chats chats}
            response @(httpkit-client/post
                       chat-auth-remote
                       {:headers {"authorization" authorization
                                  "content-type" "application/json"}
                        :method :post
                        :body (json/generate-string body)})]
        (if (= (:status response) 200)
          (swap! auth
                 assoc
                 authorization
                 {:expires (time/plus (time/now) (time/minutes chat-auth-cache-ttl))
                  :chats chats})
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
        (try
          (let [req (assoc req :body (json/parse-string (slurp (:body req)) keyword))
                authorization (get (:headers req) "authorization")]
            (if (is-authorized req)
              (if (= action :post)
                (case uri
                  "/" (batch-operation (:body req))
                  (let [{:keys [action data]} (:body req)
                        [_ filename](str/split uri #"/")]
                    (case action
                      "deleteMessage" (do
                                        (cache/delete-message filename data authorization)
                                        (json-resp 200 {}))
                      "createMessage" (do
                                        (cache/write-message filename data authorization)
                                        (json-resp 200 {}))
                      "syncRoom" (do
                                     (cache/sync-room filename data)
                                     (json-resp 200 {}))
                      (json-resp 422 {:error "wrong_action"
                                      :error_description "Unsupported action"}))))
                (json-resp 422 {:error "wrong_method"
                                :error_description "POST method required"}))
              (json-resp 403 {:error "unauthorized"
                              :error_description "Unauthorized request"})))
          (catch Exception e
            (json-resp 500 {:error "server_error"
                            :error_description (.getMessage e)})))))))

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
