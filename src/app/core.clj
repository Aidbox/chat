(ns app.core
  (:require [org.httpkit.server :as httpkit]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defonce topics (atom {}))

(defn setup-config [filename]
  (let [file (io/file (str "./" filename))
        writer (io/writer file :append true)
        reader (io/input-stream file)]
    (.mark reader 0)
    {:file file
     :writer writer
     :reader reader}))

(defn get-file-config [filename]
  (let [file-config (@topics filename)]
    (if file-config
      file-config
      (get (swap! topics #(assoc % filename (setup-config filename))) filename))))

(defn reload-reader [filename]
  (swap! topics #(let [{file :file reader :reader} (get % filename)]
                   (.close reader)
                   (assoc-in % [filename :reader] (io/input-stream file)))))

(defn parse-quesrystring [qs]
  (if qs
    (->> (str/split qs #"&")
         (map #(str/split % #"="))
         (into {}))
    {}))

(defn read-stream [stream _params]
  (.mark stream (+ 1 (.available stream)))
  (let [xout (java.io.ByteArrayOutputStream.)
        _ (io/copy stream xout)
        result (slurp (.toByteArray xout))]
    (.reset stream)
    result))

(defn app [req]
  (let [{uri :uri action :request-method} req
        filename (.substring uri 1)
        file-config (get-file-config filename)]
    (httpkit/with-channel req channel
      (let [file (:file file-config)
            params (parse-quesrystring (:query-string req))]
        (locking file
          (if (= action :post)
            (let [writer (:writer file-config)]
              (io/copy (:body req) writer)
              (.write writer "\n")
              (.flush writer)
              (httpkit/send! channel {:status  200
                                      :headers {"Content-Type" "text/html"}
                                      :body    ""}))
            (let [reader (:reader file-config)]
                (httpkit/send! channel {:status  200
                                        :headers {"Content-Type" "text/html"}
                                        :body    (read-stream reader params)})))
          (httpkit/close channel))))))

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
  (start-server)
  )

(comment
 (-main)
 (restart)
 (reset! topics {})

 (let [stream (io/input-stream "./foo")]
   [(count (read-stream stream {}))
   (count (read-stream stream {}))]
   )

 )
