;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.conditional-test
  (:require
   [clojure.test :refer [deftest is use-fixtures testing]]
   [juxt.spin.alpha.resource :as r]
   [juxt.spin.alpha.handler :as handler]
   [ring.mock.request :refer [request]]))

(deftest last-modified-304-test
  (let [h (handler/handler
           (reify
             r/ResourceLocator
             (locate-resource [_ uri request]
               {:juxt.http/last-modified #inst "2020-10-14"})))]

    (let [response
          (h (request :get "/"))]
      (is (= 200 (:status response))))

    (let [response
          (h
           (assoc-in
            (request :get "/")
            [:headers "if-modified-since"] "Wed, 14 Oct 2020 00:00:00 GMT"))]
      (is (= 304 (:status response))))))

;; This won't work for weak entity-tags!!!! TODO
(deftest etag-304-test
  (testing "strong tags"
    (let [h (handler/handler
             (reify
               r/ResourceLocator
               (locate-resource [_ uri request]
                 {:juxt.http/entity-tag "\"ABC\""})))]

      (let [response
            (h (request :get "/"))]
        (is (= 200 (:status response))))

      (let [response
            (h
             (assoc-in
              (request :get "/")
              [:headers "if-none-match"] "\"ABC\""))]
        (is (= 304 (:status response))))))

  (testing "weak tags"
    (let [h (handler/handler
             (reify
               r/ResourceLocator
               (locate-resource [_ uri request]
                 {:juxt.http/entity-tag "W/\"ABC\""})))]

      (let [response
            (h (request :get "/"))]
        (is (= 200 (:status response))))

      (let [response
            (h
             (assoc-in
              (request :get "/")
              [:headers "if-none-match"] "W/\"ABC\""))]
        (is (= 304 (:status response)))))))
