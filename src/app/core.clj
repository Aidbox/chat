(ns app.core
  (:require [app.web]
            [app.cache]
            [org.httpkit.server :as httpkit]))

(defonce server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn start-server []
  (reset! server (httpkit/run-server #'app.web/app {:port 8080})))

(defn -main[]
  (start-server))

(defn restart []
  (stop-server)
  (app.cache/reset-cache!)
  (start-server)
  )

(comment
 (restart)
 )
