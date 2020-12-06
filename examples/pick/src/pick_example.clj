;; Copyright © 2020, JUXT LTD.

(ns pick-example
  (:require
   [juxt.pick.alpha.ring :refer [pick]]
   [juxt.pick.alpha :as pick]))

(def req {:request-method :get
              :uri "/"
              :headers {"accept" "text/plain"}})

(::pick/representation
 (pick
  req
  [{::pick/content-type "text/html;charset=utf-8"}
   {::pick/content-type "text/plain;charset=utf-8"}
   {::pick/content-type "application/json"}]))
