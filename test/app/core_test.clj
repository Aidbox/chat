(ns app.core-test
  (:require [app.core :as sut]
            [app.benchmark :as utils]
            [matcho.core :as matcho]
            [clojure.test :refer :all]
            [org.httpkit.client :as httpkit]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [app.core :as app]
            [app.cache :as cache]))

(def test-room "test-room")

(defn setup []
  (utils/clear-data)
  (sut/restart)
  (utils/sync-room test-room))

(defn parse-chat [body]
  (first (json/parse-string body keyword)))

(defn parse-messages [body]
  (let [chat (first (json/parse-string body keyword))
        messages (doall (filter identity (map #(json/parse-string % keyword) (str/split (:messages chat) #"\n"))))]
    messages))

(defn rand-str []
  (apply str (repeatedly (+ (rand 12) 5) #(char (+ (rand 26) 65)))))

(defn rand-emoji []
  (if (= (int (rand 2)) 0) " 😬 😬" ""))

(defn rand-text []
  (str (rand-str) (rand-emoji)))

(defn spaces? [text]
  (every? #(= % \space) text))

(deftest dump
  (setup)
  (testing "$dump returns zip output for authorized request"
    (matcho/match @(httpkit/get "http://localhost:8080/$dump" {:headers utils/auth-headers})
      {:status 200 :headers {:content-type "application/zip"}}))

  (testing "$dump returns 403 unauthorized request"
    (matcho/match @(httpkit/get "http://localhost:8080/$dump" {:headers {"Authorization" "Basic wrongsecret"}})
      {:status 403})))

(deftest send-and-read
  (setup)
  (testing "Options request"
    (matcho/match @(httpkit/options "http://localhost:8080/fooo" {:headers utils/auth-headers})
      {:status 200}))
  (testing "Create message"
    (matcho/match (utils/insert test-room {:text "hello"}) {:status 200})
    (matcho/match (utils/insert test-room {:text "hello"}) {:status 200})
    (sut/restart)
    (matcho/match (utils/insert test-room {:text "hello"}) {:status 200})
    (matcho/match (utils/insert test-room {:text "hello"}) {:status 200})
    (sut/restart)
    (matcho/match (utils/insert test-room {:text "hello"}) {:status 200})
    (let [{:keys [status body]} (utils/read test-room)
          chat (parse-chat body)]
      (is (= status 200))
      (matcho/match chat {:users {:test-client {:viewed 5}}})))

  (testing "Read initial"
    (let [{:keys [status body]} (utils/read test-room)
          lines (parse-messages body)]
      (is (= status 200))
      (is (= (count lines) 5))
      (matcho/match lines [{:message-index 1}
                           {:message-index 2}
                           {:message-index 3}
                           {:message-index 4}
                           {:message-index 5}])))

  (testing "Read offset"
    (let [{:keys [status body]} (utils/read test-room {:offset 3})
          lines (parse-messages body)]
      (is (= status 200))
      (is (= (count lines) 2))
      (matcho/match lines [{:message-index 4}
                           {:message-index 5}])))

  (testing "Create many message"
    (doall (for [i (range 0 100)]
             (do (matcho/match (utils/insert test-room {:text (rand-text)}) {:status 200})
                 ;; We have to warm up buffer to fix init issues
                 ;; if we remove read here test will fail
                 (matcho/match (utils/read test-room) {:status 200})))))

  (testing "Read initial from big chat"
    (parse-messages (:body (utils/read test-room)))
    (let [{:keys [status body]} (utils/read test-room)
          lines (parse-messages body)]
      (is (= status 200))
      (is (= (count lines) 6))
      (matcho/match lines [{:message-index 100}
                           {:message-index 101}
                           {:message-index 102}
                           {:message-index 103}
                           {:message-index 104}
                           {:message-index 105}])))

  (testing "Read offset"
    (let [{:keys [status body]} (utils/read test-room {:offset 98})
          lines (parse-messages body)]
      (is (= status 200))
      (is (= (count lines) 1))
      (matcho/match lines [{:message-index 99}]))
    (let [{:keys [status body]} (utils/read test-room {:offset 99})
          lines (parse-messages body)]
      (is (= status 200))
      (is (= (count lines) 6))
      (matcho/match lines [{:message-index 100}
                           {:message-index 101}
                           {:message-index 102}
                           {:message-index 103}
                           {:message-index 104}
                           {:message-index 105}]))
    (let [{:keys [status body]} (utils/read test-room {:offset 103})
          lines (parse-messages body)]
      (is (= status 200))
      (is (= (count lines) 2))
      (matcho/match lines [{:message-index 104}
                           {:message-index 105}])))

  (testing "History"
    (let [{:keys [status body]} (utils/read test-room {:history 100})
          lines (parse-messages body)]
      (is (= status 200))
      (is (= (count lines) 99))))

  (testing "Viewed"
    (let [{:keys [status body]} (utils/read test-room {:offset 103, :viewed 103})
          chat (first (json/parse-string body keyword))]
      (is (= status 200))
      (is (= (get-in chat [:users :test-client :viewed]) 103)))
    (sut/restart)
    (let [{:keys [status body]} (utils/read test-room)
          chat (first (json/parse-string body keyword))]
      (is (= status 200))
      (is (= (get-in chat [:users :test-client :viewed]) 103))))
  (testing "update room info"
    (let [room-name "room-info-test"]
      (utils/sync-room room-name {:meta :data})
      (matcho/match (utils/insert room-name {:text "hello"}) {:status 200})
      (matcho/match (utils/insert room-name {:text "hello"}) {:status 200})
      (let [{:keys [status body]} (utils/read room-name {:viewed 2})
            chat (first (json/parse-string body keyword))]
        (is (= status 200))
        (matcho/match chat  {:meta "data" :users {:test-client {:viewed 2}}}))

      (utils/sync-room room-name {:meta :foo :bar :baz})
      (let [{:keys [status body]} (utils/read room-name {})
            chat (first (json/parse-string body keyword))]
        (is (= status 200))
        (matcho/match chat  {:meta "foo" :bar "baz" :users {:test-client {:viewed 2}}}))

      (utils/sync-room room-name {:empty :data :users {:superadmin {:viewed 0}
                                                       :test-client {:viewed 0}}})
      (let [{:keys [status body]} (utils/read room-name {})
            chat (first (json/parse-string body keyword))]
        (is (= status 200))
        (matcho/match chat  {:empty "data" :users {:test-client {:viewed 2}
                                                   :superadmin {:viewed 0}}}))

      (utils/sync-room room-name {:data :empty :users {:superadmin {:viewed 0}}})
      (let [{:keys [status body]} (utils/read room-name {} "superadmin")
            chat (first (json/parse-string body keyword))]
        (is (= status 200))
        (is (nil? (get-in chat [:users :test-client])))
        (matcho/match chat  {:data "empty" :users {:superadmin {:viewed 0}}}))))
  (testing "delete message"
    (matcho/match (utils/delete test-room 101) {:status 200})
    (let [{:keys [status body]} (utils/read test-room {:offset 99})
          lines (parse-messages body)]
      (is (= status 200))
      (let [target (first (filter #(= (:message-index %) 101) lines))
            action (first (filter #(= (:message-index %) 106) lines))]
        (is (spaces? (:text target)))
        (is (not (nil? action))))))
  (testing "199 offset edge case"
    (doall (for [i (range 0 93)]
             (do (matcho/match (utils/insert test-room {:text (rand-text)}) {:status 200})
                 ;; We have to warm up buffer to fix init issues
                 ;; if we remove read here test will fail
                 (matcho/match (utils/read test-room) {:status 200}))))
    (let [{:keys [status body]} (utils/read test-room {:offset 199})
          lines (parse-messages body)]
      (is (= status 200))
      (is (= (count lines) 0))))
  (testing "extra offset anfter reload"
    (doall (for [i (range 0 15)]
             (do (matcho/match (utils/insert test-room {:text (str "hello 12345678 " i)}) {:status 200})
                 ;; We have to warm up buffer to fix init issues
                 ;; if we remove read here test will fail
                 (matcho/match (utils/read test-room) {:status 200}))))
    (let [{:keys [status body]} (utils/read test-room)
          lines (parse-messages body)]
      (is (= status 200))
      (is (= (count lines) 15)))
    (sut/restart)
    (matcho/match (utils/insert test-room {:text "message after reload"}) {:status 200})
    (doall (for [_ (range 0 100)]
          ;; We have to warm up buffer to fix init issues
          ;; if we remove read here test will fail
             (utils/read test-room)))
    (let [{:keys [status body]} (utils/read test-room)
          lines (parse-messages body)]
      (is (= status 200))
      (is (= (count lines) 16)))))

(deftest concurent-send
  (setup)
  (testing "concurent send"
    (doall
     (pmap
      (fn [_i] (doall (for [_i (range 0 100)]
                        (do (matcho/match (utils/insert test-room {:text (rand-text)}) {:status 200})
                 ;; We have to warm up buffer to fix init issues
                 ;; if we remove read here test will fail
                            (matcho/match (utils/read test-room) {:status 200})))))
      (range 0 100)))
    (let [{:keys [status body]} (utils/read test-room)
          lines (parse-messages body)
          last-line (first lines)]

      (is (= status 200))
      (is (= (:message-index last-line) 10000)))))

(deftest anonimyze-author
  (setup)
  (testing "anonimyze author fields except id"
    (matcho/match (utils/insert
                   test-room
                   {:text (rand-text)}
                   {:name "Test Author"
                    :active true
                    :age 37
                    :designation "practitioner"})
      {:status 200})
    (matcho/match (utils/author-anonymize) {:status 200 :body "{\"chat-ids\":[\"test-room\"]}"})
    (let [{:keys [status body]} (utils/read test-room)
          lines (parse-messages body)
          last-line (last lines)
          author (:author last-line)]
      (is (= status 200))
      (matcho/match author {:id "test-client"
                            :active true
                            :age 37
                            :name spaces?
                            :designation spaces?}))))

(deftest utils
  (testing "find chats by user id"
    (def data {"test-room-1"
               {:room-data
                {:users
                 {:test-client
                  {:viewed 1
                   :typing false
                   :last-active "2020-11-23T08:51:30.585Z"}}}
                :index-cache
                {:lines-count 1, :length 181, :last-index 0, :line-index {0 0}}}
               "test-room-2"
               {:room-data
                {:users
                 {:another-user
                  {:viewed 1
                   :typing false
                   :last-active "2020-11-23T08:51:30.585Z"}}}
                :index-cache
                {:lines-count 1, :length 181, :last-index 0, :line-index {0 0}}}
               "test-room-3"
               {:room-data
                {:users
                 {:test-client
                  {:viewed 1
                   :typing false
                   :last-active "2020-11-23T08:51:30.585Z"}}}
                :index-cache
                {:lines-count 1, :length 181, :last-index 0, :line-index {0 0}}}})
    (is (= (cache/find-chats-by-user data :test-client) '("test-room-1" "test-room-3")))))
