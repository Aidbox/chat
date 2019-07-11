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
      (get (swap! topics #(assoc % filename (persist/load-config filename))) filename))))

(defn create-room [filename room-data]
  (swap! topics #(assoc % filename (persist/init-config filename room-data))))

(defn write-message [filename message]
  (let [file-config (get-file-config filename)
        file (get file-config :file)]
    (locking file
      (let [message (assoc message
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

(defn update-user-info [filename userId viewed typing]
  (get-in
   (swap! topics
          update-in [filename :room-data :users userId]
          assoc :viewed viewed
                :typing typing
                :last-active (time/now))
   [filename :room-data :users]))

(defn read-messages [filename {:keys [userId offset history viewed typing]}]
  (let [file-config (get-file-config filename)]
    (locking (:file file-config)
      (let [room-data (update-user-info filename userId viewed typing)]
        (persist/write-room-data file-config room-data)
        {:users room-data
         :messages (persist/read-stream file-config offset history)}))))

(comment
  (get-in @topics ["test-room" :room-data])

  )
