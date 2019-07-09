(ns app.persist
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json])
  (:import java.io.File
           java.io.InputStream
           java.io.Writer))

(def index-step 100)

(defn count-extra-lines [file offset]
  (with-open [stream (io/reader file)]
    (.skip stream offset)
    (loop [start 0]
      (let [cur-char (.read stream)]
        (if (not= cur-char -1)
          (recur
           (if (= cur-char 10)
             (+ 1 start)
             start))
          start)))))

(defn load-index-cache [file ^java.io.File index-file]
  (try
    (if (.exists index-file)
      (let [lines (str/split (slurp index-file) #"\n")
            pairs (if (= lines [""])
                    [[0 0]]
                    (->> lines
                        (map #(str/split % #" "))
                        (map (fn [[k v]]
                               [(Integer/parseInt k)
                                (Integer/parseInt v)]))
                        (into [[0 0]])))
            [last-index last-offset] (last pairs)
            line-index (into {} pairs)]
        {:lines-count (+ last-index (count-extra-lines file last-offset))
         :length (with-open [reader (io/input-stream file)]
                   (.available reader))
         :last-index last-index
         :line-index line-index})
      {:lines-count (count-extra-lines file 0)
       :length (with-open [reader (io/input-stream file)]
                 (.available reader))
       :last-index 0
       :line-index {}})
  (catch Exception e nil)))

(defn setup-config [filename]
  (let [base-filename (str "./data/" filename)
        file (io/file (str base-filename ".data"))
        index-file (io/file (str base-filename ".index"))
        index-cache (load-index-cache file index-file)
        index-cache (if index-cache
                      index-cache
                      {:lines-count 0
                       :last-index 0
                       :length 0
                       :line-index {0 0}})
        index-writer (io/writer index-file :append true)
        writer (io/writer file :append true)
        reader (io/input-stream file)]
    {:file file
     :index-cache index-cache
     :index-writer index-writer
     :writer writer
     :reader reader}))

(defn read-chunk [^java.io.InputStream stream line-index start-offset stop-offset & [extra-skip]]
  (.mark stream (+ 1 (.available stream)))
  (let [start-binary-offset (get line-index start-offset)
        stop-binary-offset (get line-index stop-offset (.available stream))
        length (- stop-binary-offset start-binary-offset)
        out-stream (java.io.StringWriter.)
        bytes (byte-array (min length (.available stream)))]
    (.skip stream start-binary-offset)
    (.read stream bytes)
    (io/copy bytes out-stream)
    (.reset stream)
    (str/join "\n" (drop (or extra-skip 0) (str/split (.toString out-stream) #"\n")))))

(defn read-history [^java.io.InputStream stream line-index history-index]
  (read-chunk stream line-index (- history-index index-step) history-index))

(defn read-stream [{:keys [^java.io.InputStream reader index-cache file]} offset history]
  (locking file (let [line-index (:line-index index-cache)]
     (if history
       (read-history reader line-index history)
       (let [initial (nil? offset)
             offset (if (nil? offset)
                      (:lines-count index-cache)
                      (if (< offset index-step) offset (+ offset 1)))
             start-offset (* (int (/ offset index-step)) index-step)
             stop-offset (+ start-offset index-step)
             extra (- offset start-offset)
             stop-offset (if (= extra 99) (+ stop-offset index-step) stop-offset)]
         (read-chunk reader line-index start-offset stop-offset (when-not initial extra)))))))

(defn write-index-stream [{:keys [^java.io.Writer index-writer]} count length]
  (let [need-index (= 0 (mod count index-step))]
    (when need-index
      (do
        (.write index-writer (str count " " length "\n"))
        (.flush index-writer)
        {count length}))))

(defn write-data-stream [{:keys [^java.io.Writer writer]} message]
  (let [data (json/generate-string message)
        writed (+ (count data) 1)]
    (.write writer data)
    (.write writer "\n")
    (.flush writer)
    writed))
