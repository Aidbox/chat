(ns app.core-test
  (:require [app.core :as sut]
            [app.benchmark :as utils]
            [matcho.core :as matcho]
            [clojure.test :refer :all]
            [org.httpkit.client :as httpkit]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [app.core :as app]))

(def test-room "test-room")

(defn setup[]
  (utils/clear-data)
  (sut/restart)
  (utils/create-room test-room)
  )

(defn parse-chat [body]
  (first (json/parse-string (slurp body) keyword)))

(defn parse-messages [body]
  (let [chat (first (json/parse-string (slurp body) keyword))
        messages (map #(json/parse-string % keyword) (str/split (:messages chat) #"\n"))]
    messages))

(deftest send-and-read
  (setup)
  (testing "Create message"
    (matcho/match (utils/insert test-room {:text "hello"}) {:status 200})
    (matcho/match (utils/insert test-room {:text "hello"}) {:status 200})
    (matcho/match (utils/insert test-room {:text "hello"}) {:status 200})
    (matcho/match (utils/insert test-room {:text "hello"}) {:status 200})
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
             (do (matcho/match (utils/insert test-room {:text "hello 12345678"}) {:status 200})
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
          chat (first (json/parse-string (slurp body) keyword))]
      (is (= status 200))
      (is (= (get-in chat [:users :test-client :viewed]) 103)))
    (sut/restart)
    (let [{:keys [status body]} (utils/read test-room)
          chat (first (json/parse-string (slurp body) keyword))]
      (is (= status 200))
      (is (= (get-in chat [:users :test-client :viewed]) 103))))
  (testing "update room info"
    (let [room-name "room-info-test"]
      (utils/create-room room-name {:meta :data})
      (matcho/match (utils/insert room-name {:text "hello"}) {:status 200})
      (matcho/match (utils/insert room-name {:text "hello"}) {:status 200})
      (let [{:keys [status body]} (utils/read room-name {:viewed 2})
            chat (first (json/parse-string (slurp body) keyword))]
        (is (= status 200))
        (matcho/match chat  {:meta "data" :users {:test-client {:viewed 2}}}))
      (utils/update-room room-name {:room-data {:meta :foo :bar :baz}})
      (let [{:keys [status body]} (utils/read room-name {})
            chat (first (json/parse-string (slurp body) keyword))]
        (is (= status 200))
        (matcho/match chat  {:meta "foo" :bar "baz" :users {:test-client {:viewed 2}}}))
      (utils/update-room room-name {:room-data {:empty :data} :new-users {:superadmin {:viewed 0}}})
      (let [{:keys [status body]} (utils/read room-name {})
            chat (first (json/parse-string (slurp body) keyword))]
        (is (= status 200))
        (matcho/match chat  {:empty "data" :users {:test-client {:viewed 2}
                                                   :superadmin {:viewed 0}}}))

      (utils/update-room room-name {:room-data {:data :empty} :remove-users ["test-client"]})
      (let [{:keys [status body]} (utils/read room-name {})
            chat (first (json/parse-string (slurp body) keyword))]
        (is (= status 200))
        (matcho/match chat  {:data "empty" :users {:superadmin {:viewed 0}}})))))
