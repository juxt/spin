;; Copyright © 2020, JUXT LTD.

(ns user
  (:require
   [ring.adapter.jetty :refer [run-jetty]]
   [demo :refer [handler]]))

(defonce server (run-jetty #'handler {:port 8080 :join? false}))
