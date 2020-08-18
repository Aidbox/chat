(ns app.dump
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.util.zip ZipEntry ZipOutputStream)))

(defn zip-folder
  [p]
  (let [baos (java.io.ByteArrayOutputStream.)]
    (with-open [zip (ZipOutputStream. baos)]
      (doseq [f (file-seq (io/file p)) :when (.isFile f)]
        (.putNextEntry zip (ZipEntry. (str/replace-first (.getPath f) p "") ))
        (io/copy f zip)
        (.closeEntry zip))
      (.close zip))
    (.toByteArray baos)))

(defn dump []
  (zip-folder "./data"))

(comment
 (dump)
)