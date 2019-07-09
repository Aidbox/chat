(ns app.core-test
  (:require [app.core :as sut]
            [clojure.test :refer :all]))


(deftest addition
  (testing "2 + 2 = 5"
    (is (= (+ 2 2) 5))))
