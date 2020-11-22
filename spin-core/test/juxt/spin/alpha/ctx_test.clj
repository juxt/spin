;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.ctx-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.spec.test.alpha :as stest]
   [juxt.spin.alpha.ctx :as ctx]
   [juxt.spin.alpha :as spin]))

(stest/instrument `ctx/locate-resource)

(defn response-for [ctx request keyseq]
  (select-keys
   ((ctx/handler ctx) request)
   keyseq))

(defn request [method path]
  {:ring.request/method method
   :ring.request/path path})

(deftest response-test
  (testing "responds 404 if no locate-resource callback"
    (is
     (=
      {:status 404}
      (response-for
       #::spin
       {}
       (request :get "/")
       [:status]))))

  (testing "responds 404 if locate-resource returns nil"
    (is
     (=
      {:status 404}
      (response-for
       #::spin
       {:locate-resource
        (fn [_] nil)}
       (request :get "/")
       [:status]))))

  (testing "responds 404 if locate-resource returns nil"
    (is
     (=
      {:status 404}
      (response-for
       #::spin
       {:locate-resource
        (fn [_] nil)}
       (request :get "/")
       [:status])))))
