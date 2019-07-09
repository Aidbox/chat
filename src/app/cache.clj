(ns app.cache
  (:require [app.persist :as persist]
            [clj-time.core :as time]))


(defonce ^:private topics (atom {}))

(defn reset-cache! []
  (reset! topics {}))

(defn update-cache-index [filename data]
  (swap! topics assoc-in [filename :index-cache] data))

(defn get-file-config [filename]
  (let [file-config (@topics filename)]
    (if file-config
      file-config
      (get (swap! topics #(assoc % filename (persist/setup-config filename))) filename))))

(defn write-message [filename message]
  (let [file-config (get-file-config filename)
        file (get file-config :file)]
    (locking file
      (let[message (assoc message
                          :message-index (+ 1 (get-in file-config [:index-cache :lines-count]))
                          :timestamp (str (time/now)))
           writed (persist/write-data-stream file-config message)
           {:keys [lines-count length line-index last-index]} (get file-config :index-cache)
           new-index (persist/write-index-stream file-config (+ lines-count 1) length)]
        (update-cache-index filename
                            {:lines-count (+ 1 lines-count)
                             :length (+ writed length)
                             :last-index (or (second new-index) last-index)
                             :line-index (into line-index new-index)})))))

(defn read-messages [filename offset history]
  (let [file-config (get-file-config filename)]
    (persist/read-stream file-config offset history)))
