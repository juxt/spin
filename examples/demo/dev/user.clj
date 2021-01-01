;; Copyright © 2020, JUXT LTD.

(ns user
  (:require
   [ring.adapter.jetty :refer [run-jetty]]
   [demo.app :refer [*database]]
   [demo.handler :refer [make-handler]]))

(defonce server
  (run-jetty
   (fn [req] ((#'make-handler *database) req))
   {:port 8080 :join? false}))
