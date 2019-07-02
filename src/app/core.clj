(ns app.core
  (:require [org.httpkit.server :as httpkit]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import java.io.File
           java.io.InputStream
           java.io.Writer))

(set! *warn-on-reflection* true)

(def index-step 100)

(defonce topics (atom {}))

(defn count-extra-lines [file offset]
  (with-open [stream (io/reader file)]
    (.skip stream offset)
    (loop [start 0
           prev-char -1]
      (let [cur-char (.read stream)]
        (if (not= cur-char -1)
          (recur
           (if (and (= prev-char 125) (= cur-char 123))
             (+ 1 start)
             start)
           cur-char)
          start)))))

(defn load-index-cache [file ^java.io.File index-file]
  (try
    (if (.exists index-file)
      (with-open [reader (io/reader index-file)]
        (let [lines (-> reader
                        slurp
                        (str/split #"\n"))
              pairs (->> lines
                         (map #(str/split % #" "))
                         (map (fn [[k v]]
                                [(Integer/parseInt k)
                                 (Integer/parseInt v)])))
              [last-index last-offset] (last pairs)
              line-index (into {} pairs)]
          {:lines-count (+ last-index (count-extra-lines file last-offset))
           :length (with-open [reader (io/input-stream file)]
                     (.available reader))
           :last-index last-index
           :line-index line-index}))
      nil)
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
                       :line-index {}})
        index-writer (io/writer index-file :append true)
        writer (io/writer file :append true)
        reader (io/input-stream file)]
    {:file file
     :index-cache index-cache
     :index-writer index-writer
     :writer writer
     :reader reader}))

(defn get-file-config [filename]
  (let [file-config (@topics filename)]
    (if file-config
      file-config
      (get (swap! topics #(assoc % filename (setup-config filename))) filename))))

(defn update-cache-index [filename fun]
  (swap! topics update-in [filename :index-cache] fun))

(defn parse-quesrystring [qs]
  (if qs
    (->> (str/split qs #"&")
         (map #(str/split % #"="))
         (into {}))
    {}))

(defn read-stream [filename ^java.io.InputStream stream line-index {offset "offset" :or {offset "0"}}]
  (.mark stream (+ 1 (.available stream)))
  (let [offset (Integer/parseInt offset)
        offset (if (= offset 0)
                 (- (get-in @topics [filename :index-cache :last-index]) 50)
                 offset)
        offset (if ( < offset 0) 0 offset)]
    (when (> offset 0)
      (let [base-offset (* (int (/ offset index-step)) index-step)
            manual-offset (- offset base-offset)
            byte-offset (get line-index base-offset 0)]
        (.skip stream byte-offset)
        (loop [start 0
               prev-char (if (= base-offset 0) 125 -1)]
          (when (< start manual-offset)
            (let [cur-char (.read stream)]
              (when (not= cur-char -1)
                (recur
                 (if (and (= prev-char 125) (= cur-char 123))
                   (+ 1 start)
                   start)
                 cur-char)))))))
    (let [out-stream (java.io.StringWriter.)
          _ (io/copy stream out-stream)
          result (.toString out-stream)]
      (.reset stream)
      (if (and (> (count result) 0) (not= (get result 0) \{))
        (str "{" result)
        result))))

(defn index []
  {:status 200
   :body (slurp "./resources/html/index.html")}
  )

(defn app [req]
  (let [{uri :uri action :request-method} req
        filename (subs uri 1)
        file-config (get-file-config filename)]
    (case uri
      "/" (index)
      (httpkit/with-channel req channel
        (let [file (:file file-config)
              params (parse-quesrystring (:query-string req))]
          (locking file
            (if (= action :post)
              (let [^java.io.Writer writer (:writer file-config)
                    ^java.io.Writer index-writer (:index-writer file-config)
                    index-file (:index-file file-config)
                    data (json/parse-string (slurp (:body req)))
                    data (json/generate-string (assoc data :message-index (+ 1 (get-in @topics [filename :index-cache :lines-count]))))
                    new-data-length (count data)]
                (update-cache-index filename (fn [{:keys [lines-count length line-index last-index]}]
                                               (let [count (+ lines-count 1)
                                                     need-index (= 0 (mod count index-step))]
                                                 {:lines-count count
                                                  :length (+ new-data-length length)
                                                  :last-index (if need-index count last-index)
                                                  :line-index (if need-index
                                                                (do
                                                                  (.write index-writer (str count " " length "\n"))
                                                                  (.flush index-writer)
                                                                  (assoc line-index count length)
                                                                  )
                                                                line-index)})))
                (.write writer data)
                (.flush writer)
                (httpkit/send! channel {:status  200
                                        :headers {"Content-Type" "text/html"
                                                  "Access-Control-Allow-Origin" "*"}
                                        :body    ""}))
              (let [reader (:reader file-config)]
                (httpkit/send! channel {:status  200
                                        :headers {"Content-Type" "text/html"
                                                  "Access-Control-Allow-Origin" "*"}
                                        :body    (read-stream filename reader (get-in @topics [filename :index-cache :line-index]) params)})))
            (httpkit/close channel)))))))

(defonce server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn start-server []
  (reset! server (httpkit/run-server #'app {:port 8080})))

(defn -main[]
  (start-server))

(defn restart []
  (stop-server)
  (reset! topics {})
  (start-server)
  )

(comment
 (-main)
 (restart)
 )
