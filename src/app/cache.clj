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
  (let [file (get (get-file-config filename) :file)]
    (locking file
      (let [file-config (get-file-config filename) ;; if we was locked,
                                                   ;; cache could be changed
                                                   ;; we need to reload cache
            message (assoc message
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

(defn get-persist-data [users]
  (->> users
       (map (fn [[id data]] [id (select-keys data [:viewed])]))
       (into {})))

(defn update-user-info [filename userId viewed typing]
  (let [old-room-data (get-in @topics [filename :room-data])
        old (get-persist-data (get-in @topics [filename :room-data :users]))
        new (get-persist-data (get-in
                               (swap! topics
                                      update-in [filename :room-data :users userId]
                                      assoc :viewed (or viewed (get-in @topics [filename :room-data :users userId :viewed]))
                                      :typing typing
                                      :last-active (time/now))
                               [filename :room-data :users]))]
    (when (not= old new)
      (assoc old-room-data :users new))))

(defn read-messages [filename {:keys [userId offset history viewed typing]}]
  (let [file-config (get-file-config filename)]
    (locking (:file file-config)
      (let [persist-room-data (update-user-info filename userId viewed typing)]
        (when persist-room-data
          (persist/write-room-data file-config persist-room-data))
        (assoc
         (get-in @topics [filename :room-data])
         :messages (persist/read-stream file-config offset history))))))

(comment
  (get-in @topics ["test-room" :room-data])
  (get-in @topics ["benchmark" :room-data])
  
  )
