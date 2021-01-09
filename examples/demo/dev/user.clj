;; Copyright © 2020-2021, JUXT LTD.

(ns user
  (:require
   [ring.adapter.jetty :refer [run-jetty]]
   [demo :refer [make-handler wrap-exception-handler *database]]))

(defonce server
  (run-jetty
   (fn [req] ((->
               (#'make-handler *database)
               wrap-exception-handler) req))
   {:port 8080 :join? false}))
