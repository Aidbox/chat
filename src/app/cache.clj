(ns app.cache
  (:require [app.persist :as persist]
            [org.httpkit.client :as httpkit-client]
            [cheshire.core :as json]
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

(defn update-room [filename room]
  (let [file (get (get-file-config filename) :file)]
    (locking file
      (let [;; if we was locked,
            ;; cache could be changed
            ;; we need to reload cache
            file-config (get-file-config filename)
            {data :room-data add-users :new-users remove-users :remove-users} room
            dissoc-list #(apply dissoc )
            room-data (-> file-config
                          :room-data
                          (merge data)
                          (update :users merge add-users)
                          (update :users #(apply dissoc % remove-users)))
            room-data (merge (:room-data file-config) room-data) ;; keep chat useruser information
            ]
        (persist/update-room-info filename room-data)
        (swap! topics #(assoc-in % [filename :room-data] room-data))))))

(def chat-push-notification (get (System/getenv) "CHAT_PUSH_NOTIFICATION"))

(defn send-notification [chat-name offline-users message authorization]
  (when chat-push-notification
    (httpkit-client/post
     chat-push-notification
     {:headers {"authorization" authorization
                "content-type" "application/json"}
      :method :post
      :body (json/generate-string {:users offline-users :message message :chat chat-name})}
     (fn [{:keys [status body]}]
       (when (not= status 200)
         (println status body))))))

(defn notify-offline-users [chat-name file-config message authorization]
  (let [now (time/now)
        offline-users (filter
                       (complement nil?)
                       (for [[id {:keys [viewed last-active]}] (get-in file-config [:room-data :users])]
                         (if (nil? last-active)
                           id
                           (when (> (time/in-msecs (time/interval last-active now)) 2000)
                             id))))]
    (when (count offline-users)
      (send-notification chat-name offline-users message authorization))))

(defn get-persist-data [users]
  (->> users
       (map (fn [[id data]] [id (select-keys data [:viewed])]))
       (into {})))

(defn update-user-viewed [filename userId viewed]
  (let [old-room-data (get-in @topics [filename :room-data])
        old (get-persist-data (get-in @topics [filename :room-data :users]))
        new (get-persist-data (get-in
                               (swap! topics
                                      update-in [filename :room-data :users userId]
                                      assoc :viewed viewed
                                      :last-active (time/now))
                               [filename :room-data :users]))]
    (when (not= old new)
      (assoc old-room-data :users new))))

(defn write-message [filename message authorization]
  (let [file (get (get-file-config filename) :file)]
    (locking file
      (let [;; if we was locked,
            ;; cache could be changed
            ;; we need to reload cache
            file-config (get-file-config filename)
            userId (keyword (get-in message [:author :id]))
            in-chat (get-in file-config [:room-data :users userId])]
        (if in-chat
          (let [message-index (+ 1 (get-in file-config [:index-cache :lines-count]))
                message (assoc message
                               :message-index message-index
                               :timestamp (str (time/now)))
                writed (persist/write-data-stream file-config message)
                {:keys [lines-count length line-index last-index]} (get file-config :index-cache)
                new-index (persist/write-index-stream file-config (+ lines-count 1) length)]
            (notify-offline-users filename file-config message authorization)
            (update-cache-index filename
                                {:lines-count (+ 1 lines-count)
                                 :length (+ writed length)
                                 :last-index (or (second new-index) last-index)
                                 :line-index (into line-index new-index)})
            (let [persist-room-data (update-user-viewed filename userId message-index)]
              (when persist-room-data
                (persist/write-room-data file-config persist-room-data))))
          (throw (Exception. "User isn't in chat while writing")))))))

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
      (let [;; if we was locked,
            ;; cache could be changed
            ;; we need to reload cache
            file-config (get-file-config filename)
            in-chat (get-in file-config [:room-data :users userId])]
        (if in-chat
          (let [persist-room-data (update-user-info filename userId viewed typing)]
            (when persist-room-data
              (persist/write-room-data file-config persist-room-data))
            (assoc
             (get-in @topics [filename :room-data])
             :messages (persist/read-stream file-config offset history)))
          (throw (Exception. "User is not in chat while reading")))))))

(comment
  (get-in @topics ["test-room" :room-data])
  (get-in @topics ["benchmark" :room-data])

  (:id (:chat (:room-data @debug)))

  )

