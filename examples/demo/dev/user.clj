;; Copyright © 2020, JUXT LTD.

(ns user
  (:require
   [ring.adapter.jetty :refer [run-jetty]]
   [demo :refer [make-handler *database]]))

(defonce server
  (run-jetty
   (fn [req] ((#'make-handler *database) req))
   {:port 8080 :join? false}))
