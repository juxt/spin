;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.handler-test
  (:require
   [clojure.test :refer [deftest is use-fixtures]]
   [juxt.spin.alpha.resource :as r]
   [juxt.spin.alpha.handler :as handler]
   [ring.mock.request :refer [request]]))

(deftest method-not-allowed-test
  (let [*response (promise)
        h (#'handler/invoke-method
           nil
           nil
           #{:get :head :put :post :delete :options})]
    (h (request :post "/")
       (fn [r] (deliver *response r))
       (fn [_]))
    (let [response (deref *response 0 :timeout)]
      (is (= 405 (:status response)))
      (is (= "GET" (get-in response [:headers "allow"])))
      response)))

;; TODO: Test other combinations, including where AllowedMethods is implemented.



(deftest nil-resource-test
  ;; Should return 404 if nil resource
  (let [h (handler/handler
           (reify
             r/ResourceLocator
             (locate-resource [_ uri request]
               (cond
                 (.endsWith uri "/connor") {:name "Connor"}
                 :else nil)))
           nil)]

    (is (= 200 (:status (h (request :get "/connor")))))
    (is (= 404 (:status (h (request :get "/malcolm")))))))
