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
  (sut/restart))

(defn insert [message]
  @(httpkit/post (str "http://localhost:8080/" test-room)
                 {:body (json/generate-string message)}))

(defn read [& [offset]]
  @(httpkit/get (str "http://localhost:8080/" test-room) (when offset {:query-params {:offset offset}})))

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
          lines (map #(json/parse-string % keyword) (str/split body #"\n"))]
      (is (= status 200))
      (is (= (count lines) 5))
      (matcho/match lines [{:message-index 1}
                           {:message-index 2}
                           {:message-index 3}
                           {:message-index 4}
                           {:message-index 5}])))

  (testing "Read offset"
    (let [{:keys [status body]} (read 3)
          lines (map #(json/parse-string % keyword) (str/split body #"\n"))]
      (is (= status 200))
      (is (= (count lines) 2))
      (matcho/match lines [{:message-index 4}
                           {:message-index 5}]))))
