;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.ctx-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.spin.alpha.ctx :as ctx]
   [ring.mock.request :refer [request]]))

(defn response-for [spi request keyseq]
  (select-keys
   ((ctx/handler spi) request)
   keyseq))

(deftest response-test
  (testing "responds 404 if no locate-resource callback"
    (is
     (=
      {:status 404}
      (response-for
       #:juxt.spin.alpha
       {}
       (request :get "/")
       [:status]))))

  (testing "responds 404 if locate-resource returns nil"
    (is
     (=
      {:status 404}
      (response-for
       #:juxt.spin.alpha
       {:locate-resource
        (fn [_] nil)}
       (request :get "/")
       [:status])))))
