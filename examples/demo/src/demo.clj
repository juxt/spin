;; Copyright © 2020, JUXT LTD.

(ns demo
  (:require
   [demo.app :refer [*database]]
   [demo.handler :refer [make-handler]]
   [ring.adapter.jetty :as jetty]))

(defn handler [req]
  ((make-handler *database) req))

(defn run [opts]
  (jetty/run-jetty
   (make-handler *database)
   {:port (Integer/parseInt (get opts "--port" "8080")) :join? true}))
