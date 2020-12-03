;; Copyright © 2020, JUXT LTD.

(ns juxt.spinner.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.spin.alpha :as spin]
   [juxt.spin.alpha.test-util :refer [request]]
   [juxt.spin.alpha.util :as util]
   [juxt.spinner.core :as s]))

(defn response-for
  ([h request]
   ((util/sync-adapt h) request))
  ([h request keyseq]
   (let [keyseq (cond-> keyseq (seq (filter string? keyseq)) (conj :ring.response/headers))]
     (cond-> (response-for h request)
       true
       (select-keys (filter keyword? keyseq))
       (seq (filter string? keyseq))
       (update :ring.response/headers select-keys (filter string? keyseq))))))

(deftest unknown-method-test
  (testing "501 response for unknown method"
    (is
     (=
      {:ring.response/status 501}
      (response-for
       (fn [request respond! _]
         (when-let [response (s/unknown-method? request)]
           (respond! response)))
       (request :brew "/")
       [:ring.response/status])))))

(deftest not-found-test
  (testing "a 404 response if resource is an empty map"
    (is
     (=
      {:ring.response/status 404}
      (response-for
       (fn [request respond! _]
         (when-let [response (s/unknown-method? request)]
           (respond! response))
         (case (:ring.request/method request)
           (:head :get)
           (let [representation nil]
             (when-let [response (s/not-found? representation)]
               (respond! response)))))
       (request :get "/")
       [:ring.response/status])))))

(deftest method-not-allowed-test
  (testing "a 405 response if method not allowed"
    (is
     (=
      {:ring.response/status 405}

      (response-for
       (fn [request respond! _]

         (when-let [response (s/method-not-allowed? request #{:get})]
           (respond! response)))

       (request :post "/")
       [:ring.response/status])))))

(deftest get-and-head-test
  (let [h (fn [request respond! _]

            (when-let [response (s/method-not-allowed? request #{:get})]
              (respond! response))

            (case (:ring.request/method request)
              (:head :get)
              (let [representation
                    {::spin/content-type "text/plain"
                     ::spin/content-length (count (.getBytes "Hello World!\n"))}]

                (respond!
                 (cond-> {:ring.response/status 200}
                   true (conj (s/representation->response representation))
                   (not (s/head? request)) ; when not HEAD …
                   ;; … we add the body ourselves
                   (conj {:ring.response/body "Hello World!\n"}))))))]

    (testing "a 200 response, with a body, in response to a GET request"
      (is
       (=
        {:ring.response/status 200
         :ring.response/headers {"content-length" "13"}
         :ring.response/body "Hello World!\n"}

        (response-for
         h
         (request :get "/")
         [:ring.response/status :ring.response/body "content-length"]))))

    (testing "a 200 response, without a body, in response to a HEAD request"
      (let [response
            (response-for
             h
             (request :get "/"))]
        (is (= 200 (:ring.response/status response)                     ))
        (is (= "13" (get-in response [:ring.response/headers "content-length"])))
        (is (nil? (find response :body)))))))

(deftest bad-request-test
  (testing "a 400 response if bad request"
    (is
     (=
      {:ring.response/status 400}

      (response-for
       (fn [request respond! _]
         (when-not (get-in request [:ring.request/headers "some-required-header"])
           (respond! (s/bad-request))))

       (request :get "/")
       [:ring.response/status])))))

(deftest allow-test
  (is
   (=
    {:ring.response/status 405
     :ring.response/headers {"allow" "GET, HEAD, OPTIONS"}}
    (response-for

     (fn [request respond! _]

       (when-let [response (s/method-not-allowed? request #{:get})]
         (respond! response)))

     (request :post "/")
     [:ring.response/status "allow"]))))

(deftest post-test
  (let [h (fn [request respond! _]
            (when-let [response (s/method-not-allowed? request #{:post})]
              (respond! response))

            (respond! (s/created "/new-resource")))]

    (testing "responds with 201 when new resource created"
      (is
       (=
        {:ring.response/status 201
         :ring.response/headers {"location" "/new-resource"}}
        (response-for
         h
         (request :post "/")
         [:ring.response/status "location"]))))))

(deftest response-header-date-test
  (-> (fn [_ respond! _]
        (respond! {:status 200}))
      s/wrap-add-date
      (response-for (request :get "/"))
      (get-in [:ring.response/headers "date"])
      util/parse-http-date
      inst? is))
