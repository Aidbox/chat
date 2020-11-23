(ns app.persist
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-time.core :as time]
            [clj-time.format :as format-time]
            [cheshire.core :as json])
  (:import java.io.File
           java.io.InputStream
           java.io.Writer))

(set! *warn-on-reflection* true)

(def index-step 100)

(defn count-lines [file]
  (with-open [stream (io/reader file)]
    (loop [start 0]
      (let [cur-char (.read stream)]
        (if (not= cur-char -1)
          (recur
           (if (= cur-char 10)
             (+ 1 start)
             start))
          start)))))

(defn load-index-cache [file ^java.io.File index-file]
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
        line-index (into {} pairs)
        lines-count (count-lines file)]
    {:lines-count lines-count
     :length (with-open [reader (io/input-stream file)]
               (.available reader))
     :last-index last-index
     :line-index line-index}))

(defn is-config-exists [filename]
  (let [base-filename (str "./data/" filename)
        info-file (io/file (str base-filename ".info"))]
    (.exists info-file)))

(defn load-config [filename]
  (let [base-filename (str "./data/" filename)
        info-file (io/file (str base-filename ".info"))]
    (if (.exists info-file)
      (let [index-file (io/file (str base-filename ".index"))
            file (io/file (str base-filename ".data"))
            index-cache (load-index-cache file index-file)
            index-cache (if index-cache
                          index-cache
                          {:lines-count 0
                           :last-index 0
                           :length 0
                           :line-index {0 0}})
            index-writer (io/writer index-file :append true)
            writer (io/writer file :append true)
            reader (io/input-stream file)
            room-data (json/parse-string (slurp info-file) keyword)
            room-data (update room-data :users (fn [x] (into {}
                                                             (map (fn [[k v]]
                                                                    [k (update v :last-active format-time/parse)]) x))))]
        {:file file
         :info-file info-file
         :room-data room-data
         :index-cache index-cache
         :index-writer index-writer
         :writer writer
         :reader reader})
      (throw (Exception. (str "Room " filename " doesn't exist"))))))

(defn init-config [filename room-data]
  (let [base-filename (str "./data/" filename)
        info-file (io/file (str base-filename ".info"))]
    (if (.exists info-file)
      (throw (Exception. (str "Room " filename " already exists")))
      (let [file (io/file (str base-filename ".data"))
            index-file (io/file (str base-filename ".index"))]
        (spit info-file (json/generate-string room-data))
        {:file file
         :info-file info-file
         :room-data room-data
         :index-cache {:lines-count 0
                       :last-index 0
                       :length 0
                       :line-index {0 0}}
         :index-writer (io/writer index-file :append true)
         :writer (io/writer file :append true)
         :reader (io/input-stream file)}))))

(defn close-topic [{:keys [index-writer writer reader]}]
  (.close index-writer)
  (.close writer)
  (.close reader))

(defn update-room-info [filename room-data]
  (let [base-filename (str "./data/" filename)
        info-file (io/file (str base-filename ".info"))]
    (spit info-file (json/generate-string room-data))))

(defn read-chunk [^java.io.InputStream stream max-length line-index start-offset stop-offset & [extra-skip]]
  (.mark stream (+ 1 max-length))
  (let [start-binary-offset (get line-index start-offset)
        stop-binary-offset (get line-index stop-offset max-length)]
    (if (and (not (nil? stop-binary-offset)) (not (nil? start-binary-offset)))
      (let [length (- stop-binary-offset start-binary-offset)
            out-stream (java.io.StringWriter.)
            bytes (byte-array (min length max-length))]
        (.skip stream start-binary-offset)
        (.read stream bytes)
        (io/copy bytes out-stream)
        (.reset stream)
        (str/join "\n" (drop (or extra-skip 0) (str/split (.toString out-stream) #"\n"))))
      "")))

(defn read-history [^java.io.InputStream stream max-length line-index history-index]
  (read-chunk stream max-length line-index (- history-index index-step) history-index))

(defn read-stream [{:keys [^java.io.InputStream reader index-cache ^java.io.File file]} offset history]
  (let [line-index (:line-index index-cache)
        max-length (.length file)]
    (if history
      (read-history reader max-length line-index history)
      (let [initial (nil? offset)
            offset (if (nil? offset)
                     (:lines-count index-cache)
                     (if (< offset index-step) offset (+ offset 1)))
            start-offset (* (int (/ offset index-step)) index-step)
            stop-offset (+ start-offset index-step)
            extra (- offset start-offset)
            stop-offset (if (= extra 99) (+ stop-offset index-step) stop-offset)]
        (read-chunk reader max-length line-index start-offset stop-offset (when-not initial extra))))))

(defn write-index-stream [{:keys [^java.io.Writer index-writer]} count length]
  (let [need-index (= 0 (mod count index-step))]
    (when need-index
      (do
        (.write index-writer (str count " " length "\n"))
        (.flush index-writer)
        {count length}))))

(defn write-data-stream [{:keys [^java.io.Writer writer]} message]
  (let [data (json/generate-string message)
        writed (+ (alength (.getBytes data)) 1)]
    (.write writer data)
    (.write writer "\n")
    (.flush writer)
    writed))

(defn write-room-data [{:keys [info-file]} room-data]
  (spit info-file (json/generate-string room-data)))

(defn check-index []
  (doseq [file (->> (clojure.java.io/file "./data")
                    file-seq
                    (filter #(not (.isDirectory %)))
                    (filter #(str/ends-with? (.getName %) ".data")))]
    (let [data (map json/parse-string (filter identity (str/split (slurp file) #"\n")))]
      (when (> (count data) 1)
        (let [[correct] (reduce (fn [[correct index] item]
                                  (let [message-index (get item "message-index")]
                                    (if (= index -1)
                                      [false -1]
                                      (if (> message-index index)
                                        [true message-index]
                                        [false -1]))))
                                [true 0]
                                data)]
          (if (not correct)
            (println "Wrong indexing of " (.getName file))))))))

(defn clean-text [text]
  (let [text-length (alength (.getBytes text))]
    (apply str (repeatedly text-length (constantly \space)))))

(defn delete-message [filename index]
  (let [file-path (str "./data/" filename ".data")
        source-file (io/file file-path)
        destination-file (java.io.File/createTempFile filename ".data")]
    (with-open [source (io/reader source-file)
                destination (io/writer destination-file)]
      (doseq [[current-index line] (map-indexed vector (line-seq source))]
        (if (= (+ current-index 1) index)
          (let [message (json/parse-string line keyword)
                text (:text message)
                replacing-text (clean-text text)
                replacing-line (json/generate-string (assoc message :text replacing-text))]
            (.write destination replacing-line))
          (.write destination line))
        (.write destination "\n")))
    (io/copy destination-file source-file)
    (.delete destination-file)))

(defn anonymize-messages [filename user-id]
  (let [file-path (str "./data/" filename ".data")
        source-file (io/file file-path)
        destination-file (java.io.File/createTempFile filename ".data")]
    (with-open [source (io/reader source-file)
                destination (io/writer destination-file)]
      (doseq [line (line-seq source)]
        (let [message (json/parse-string line keyword)
              author (:author message)]
          (if (= user-id (keyword (:id author)))
            (let [to-remove (disj (->> author
                                       (filter (comp string? second))
                                       (map first)
                                       set) :id)
                  replacing-author (into {} (map (fn [[key value]] [key (if (to-remove key) (clean-text value) value)]) author))
                  replacing-line (json/generate-string (assoc message :author replacing-author))]
              (.write destination replacing-line))
            (.write destination line)))
        (.write destination "\n")))
    (io/copy destination-file source-file)
    (.delete destination-file)))

(comment
  (def author-data {:id "test-id-123"
                    :name "Test Author"
                    :active true
                    :age 37
                    :designation "practitioner"})
  (let [to-remove (disj (->> author-data
                             (filter (comp string? second))
                             (map first)
                             set) :id)]
    (into {} (map (fn [[key value]] [key (if (to-remove key) (clean-text value) value)]) author-data)))

  (disj (set (map first (filter (fn [[key value]] (string? value)) author-data))) :id)
  (check-index)

  (let [filename "903d30ed-8034-4788-bf6c-c368e5691667"
        base-filename (str "./data/" filename)
        file (io/file (str base-filename ".data"))
        bytes (byte-array 1000)
        out-stream (java.io.StringWriter.)]
    (with-open [stream (io/input-stream file)]
      (.skip stream 81370)
      (.read stream bytes)
      (io/copy bytes out-stream)
      (.toString out-stream)))

  (let [room-data {:users
                   {:e323e5e6-37ef-43a8-be27-35c517ebb8f9 {:viewed 142}
                    :7e35408a-dd49-40ec-8a25-a504aae8d5b0 {:viewed 154 :typing false, :last-active "2019-12-16T06:15:39.165Z"}
                    :ac44242f-143a-4f8d-b512-839dc8f6cb7b {:viewed nil}
                    :a9912cfc-4efe-4af6-930a-973af6d5f46d {:viewed 111, :typing false, :last-active "2019-12-12T05:29:25.022Z"}}}]
    (update room-data :users (fn [x] (into {}
                                           (map (fn [[k v]]
                                                  [k (update v :last-active format-time/parse)]) x)))))

  (format-time/parse (get (json/parse-string (json/generate-string {:now (time/now)})) "now")))
