;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.conditional-test
  (:require
   [clojure.test :refer [deftest is use-fixtures testing]]
   [juxt.spin.alpha.methods :refer [http-method]]
   [juxt.spin.alpha.resource :as r]
   [juxt.spin.alpha.handler :as handler]
   [ring.mock.request :refer [request]]
   [clojure.tools.logging :as log]
   [juxt.spin.alpha.methods :as methods]))

(deftest last-modified-304-test
  (let [h (handler/handler
           (reify
             r/ResourceLocator
             (locate-resource [_ uri request]
               {:juxt.http/last-modified #inst "2020-10-14"})))]

    (let [response
          (h (request :get "/connor"))]
      (is (= 200 (:status response))))

    (let [response
          (h
           (assoc-in
            (request :get "/connor")
            [:headers "if-modified-since"] "Wed, 14 Oct 2020 00:00:00 GMT"))]
      (is (= 304 (:status response))))))

#_(deftest etag-304-test
  (let [h (handler/handler
           (reify
             r/ResourceLocator
             (locate-resource [_ uri request]
               {:juxt.http/entity-tag "W/\"V1\""})))]

    (let [response
          (h (request :get "/connor"))]
      (is (= 200 (:status response))))

    (let [response
          (h
           (assoc-in
            (request :get "/connor")
            [:headers "if-none-match"] "W/\"V1\", W/\"V2\""))]
      (is (= 304 (:status response))))))
