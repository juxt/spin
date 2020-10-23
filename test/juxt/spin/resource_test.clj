;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.resource-test
  (:require
   [clojure.test :refer [deftest is use-fixtures testing]]
   [juxt.spin.alpha.handler :refer [handler]]
   [juxt.spin.alpha.resource :as r]
   [ring.mock.request :refer [request]]
   [juxt.reap.alpha.api :as reap])
  (:import
   (java.util.logging Logger Level Handler)))

(def ^:dynamic  *log-records* [])

(defn with-log-capture [f]
  (binding [*log-records* (atom [])]
    (let [juxt-logger (Logger/getLogger "juxt")
          h (proxy [Handler] []
              (close [] nil)
              (flush [] nil)
              (publish [lr] (swap! *log-records* conj lr)))
          old-level (.getLevel juxt-logger)]
      (try
        (.addHandler juxt-logger h)
        (.setLevel juxt-logger Level/FINEST)
        (f)
        (finally
          (.setLevel juxt-logger old-level)
          (.removeHandler juxt-logger h))))))

(use-fixtures :each with-log-capture)

(deftest get-with-body-default-status-test
  ((handler
    (reify
      r/GET
      (get-or-head [_ server resource response request respond raise]
       (respond (conj response [:body "Hello World!"])))))
   (request :get "/")
   (fn [response]
     (is (= 200 (:status response)))
     (is (= "Hello World!" (:body response))))
   (fn [t] (throw t))))

(deftest get-with-body-explicit-status-test
  ((handler
    (reify
      r/GET
      (get-or-head [_ server resource response request respond raise]
        (respond
         (merge
          response
          {:status 200
           :body "Hello World!"})))))
   (request :get "/")
   (fn [response]
     (is (= 200 (:status response)))
     (is (= "Hello World!" (:body response))))
   (fn [t] (throw t))))

(deftest get-with-body-explicit-not-ok-test
  ((handler
    (reify
      r/GET
      (get-or-head [_ server resource response request respond raise]
        (respond
         (merge
          response
          {:status 400
           :body "Bad request!"})))))
   (request :get "/")
   (fn [response]
     (is (= 400 (:status response)))
     (is (= "Bad request!" (:body response))))
   (fn [t] (throw t))))

(deftest get-with-content-response
  ((handler
    (reify
      r/ContentResponse
      (respond-with-content [_ server resource representations response request respond raise]
        (respond
         (-> response
             (update :headers (fnil conj {}) ["content-type" "text/plain;charset=utf8"])
             (conj [:body "Hello World!"]))))))
   (request :get "/")
   (fn [response]
     (is (= 200 (:status response)))
     (is (= "text/plain;charset=utf8" (get-in response [:headers "content-type"])))
     (is (= "Hello World!" (:body response))))
   (fn [t] (throw t))))

(deftest get-with-no-representation-404-test
  ((handler
    (reify
      r/ResourceLocator
      (locate-resource [_ uri request]
        nil)
      r/ContentVariants
      (available-variants [_ server resource response]
        (case (:status response)
          200 []
          404 [{:juxt.http/content-type "text/plain;charset=utf8"}]))))
   (request :get "/")
   (fn [response]
     (is (= 404 (:status response))))
   (fn [t] (throw t))))

(deftest get-with-no-acceptable-representation-406-test
  ((handler
    (reify
      r/ContentVariants
      (available-variants [_ server resource response]
        (case (:status response)
          200 [:json]
          ;; Here's our error message!
          406 [{:juxt.http/content-type "text/plain;charset=utf8"}]
          (throw (ex-info "Unexpected case" {:response response}))))

      r/ContentProactiveNegotiation
      (select-representations [_ server request variants]
        (remove #(= :json %) variants))))
   (request :get "/")
   (fn [response]
     (is (= 406 (:status response))))
   (fn [t] (throw t))))

(deftest get-with-varying-content-type-test
  (let [handler
        (handler
         (reify
           r/ContentVariants
           (available-variants [_ server resource response]
             [{:juxt.http/content-type "text/html;charset=utf8"}
              {:juxt.http/content-type "application/json"}])

           r/ContentProactiveNegotiation
           (select-representations [_ server request variants]
             (is (= 2 (count variants)))
             (is (= [{:juxt.http/content-type "text/html;charset=utf8"}
                     {:juxt.http/content-type "application/json"}]
                    variants))
             ;; In the absence of a proactive negotiation algorithm, just pick the
             ;; first (the text/html variant).
             (take 1 variants))

           r/ContentResponse
           (respond-with-content [_ server resource representations response request respond raise]
             (is (= 1 (count representations)))
             (is (= {:juxt.http/content-type "text/html;charset=utf8"} (first representations)))
             (respond
              (-> response
                  (update :headers (fnil conj {}) ["content-type" (:juxt.http/content-type (first representations))])
                  (conj [:body (case (:juxt.http/content-type (first representations))
                                 "text/html;charset=utf8" "<h1>Hello World!</h1>"
                                 "application/json" "{\"message\": \"Hello World!\"}")]))))))]
    (testing "GET"
      (handler
       (request :get "/")
       (fn [response]
         (is (= 200 (:status response)))
         (is (= "<h1>Hello World!</h1>" (:body response)))
         (is (= "text/html;charset=utf8" (get-in response [:headers "content-type"])))
         (is (= "accept" (get-in response [:headers "vary"]))))
       (fn [t] (throw t))))

    (testing "HEAD"
      (handler
       (request :head "/")
       (fn [response]
         (is (= 200 (:status response)))
         (is (nil? (:body response)))
         (is (= "text/html;charset=utf8" (get-in response [:headers "content-type"])))
         (is (= "accept" (get-in response [:headers "vary"]))))
       (fn [t] (throw t))))))

(deftest vary-header-test
  (let [make-handler
        (fn [variants]
          (handler
           (reify
             r/ContentVariants
             (available-variants [_ server resource response]
               variants)

             r/ContentProactiveNegotiation
             (select-representations [_ server request variants]
               (take 1 variants)))))]
    (testing "vary by accept"
      (let [h (make-handler
               [{:juxt.http/content-type "text/html;charset=utf8"}
                {:juxt.http/content-type "application/json"}])]
        (h
         (request :get "/")
         (fn [response]
           (is (= 200 (:status response)))
           (is (= "accept" (get-in response [:headers "vary"]))))
         (fn [t] (throw t)))))

    (testing "vary by accept-language"
      (let [h (make-handler
               [{:juxt.http/content-type "text/html;charset=utf8"
                 :juxt.http/content-language "en-US"}
                {:juxt.http/content-type "text/html;charset=utf8"
                 :juxt.http/content-language "de"}])]
        (h
         (request :get "/")
         (fn [response]
           (is (= 200 (:status response)))
           (is (= "accept-language" (get-in response [:headers "vary"]))))
         (fn [t] (throw t)))))))

;; TODO: Try with a 404 content response
;; TODO: Try with a 406 content response
;; TODO: Try with a 500 content response
