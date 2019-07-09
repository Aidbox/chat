(ns app.cache
  (:require [app.persist :as persist]))


(defonce topics (atom {}))

(defn update-cache-index [filename fun]
  (swap! topics update-in [filename :index-cache] fun))

(defn get-file-config [filename]
  (let [file-config (@topics filename)]
    (if file-config
      file-config
      (get (swap! topics #(assoc % filename (persist/setup-config filename))) filename))))
