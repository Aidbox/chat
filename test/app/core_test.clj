(ns app.core-test
  (:require [app.core :as sut]
            [matcho.core :as matcho]
            [clojure.test :refer :all]
            [org.httpkit.client :as httpkit]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def test-room "test-room")

(defn setup[]
  (try
    (io/delete-file (str "./data/" test-room ".data"))
    (catch Exception e nil))
  (try
    (io/delete-file (str "./data/" test-room ".index"))
    (catch Exception e nil))
  (try
    (io/delete-file (str "./data/" test-room ".info"))
    (catch Exception e nil))
  (sut/restart)
  @(httpkit/post (str "http://localhost:8080/" test-room)
                 {:body (json/generate-string {:action "createRoom" :data {:room "metadata"}})})
  )

(defn insert [message]
  @(httpkit/post (str "http://localhost:8080/" test-room)
                 {:body (json/generate-string {:action "createMessage" :data message})}))

(defn read [& [offset]]
  (let [request {:id test-room}]
    @(httpkit/post "http://localhost:8080/"
                   {:body (json/generate-string
                           {:userId "test-client"
                            :chats [(if offset (assoc request :offset offset) request)]})})))

(defn get-messages [body]
  (let [chat (first (json/parse-string (slurp body) keyword))
        messages (map #(json/parse-string % keyword) (str/split (:messages chat) #"\n"))]
    messages))

(get-messages (:body (read)))

(deftest send-and-read
  (setup)
  (testing "Create message"
    (matcho/match (insert {:text "hello"}) {:status 200})
    (matcho/match (insert {:text "hello"}) {:status 200})
    (matcho/match (insert {:text "hello"}) {:status 200})
    (matcho/match (insert {:text "hello"}) {:status 200})
    (matcho/match (insert {:text "hello"}) {:status 200}))

  (testing "Read initial"
    (let [{:keys [status body]} (read)
          lines (get-messages body)]
      (is (= status 200))
      (is (= (count lines) 5))
      (matcho/match lines [{:message-index 1}
                           {:message-index 2}
                           {:message-index 3}
                           {:message-index 4}
                           {:message-index 5}])))

  (testing "Read offset"
    (let [{:keys [status body]} (read 3)
          lines (get-messages body)]
      (is (= status 200))
      (is (= (count lines) 2))
      (matcho/match lines [{:message-index 4}
                           {:message-index 5}])))

  (testing "Create many message"
    (doall (for [i (range 0 100)]
             (matcho/match (insert {:text "hello"}) {:status 200}))))

  (testing "Read initial from big chat"
    (get-messages (:body (read))) ;; TODO warmup buffer to fix init issues
    (let [{:keys [status body]} (read)
          lines (get-messages body)]
      (is (= status 200))
      (is (= (count lines) 6))
      (matcho/match lines [{:message-index 100}
                           {:message-index 101}
                           {:message-index 102}
                           {:message-index 103}
                           {:message-index 104}
                           {:message-index 105}])))

  (testing "Read offset"
    (let [{:keys [status body]} (read 98)
          lines (get-messages body)]
      (is (= status 200))
      (is (= (count lines) 1))
      (matcho/match lines [{:message-index 99}]))
    (let [{:keys [status body]} (read 99)
          lines (get-messages body)]
      (is (= status 200))
      (is (= (count lines) 6))
      (matcho/match lines [{:message-index 100}
                           {:message-index 101}
                           {:message-index 102}
                           {:message-index 103}
                           {:message-index 104}
                           {:message-index 105}]))
    (let [{:keys [status body]} (read 103)
          lines (get-messages body)]
      (is (= status 200))
      (is (= (count lines) 2))
      (matcho/match lines [{:message-index 104}
                           {:message-index 105}]))))

(comment
  (get-messages (:body (read 99)))
  )
