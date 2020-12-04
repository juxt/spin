;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.spin.alpha :as spin]
   [juxt.spin.alpha.util :as util]))

(defn header [request header value]
  (-> request
      (assoc-in [:headers header] value)))

(defn request [method reluri]
  (let [[_ path query] (re-matches #"([^\?]*)\??(.*)" reluri)]
    (cond-> {:request-method method}
     (seq path) (conj [:uri path])
     (seq query) (conj [:query-string query]))))

(defn response-for
  ([h request]
   (h request))
  ([h request keyseq]
   (let [keyseq (cond-> keyseq (seq (filter string? keyseq)) (conj :headers))]
     (cond-> (response-for h request)
       true
       (select-keys (filter keyword? keyseq))
       (seq (filter string? keyseq))
       (update :headers select-keys (filter string? keyseq))))))

(deftest unknown-method-test
  (testing "501 response for unknown method"
    (is
     (=
      {:status 501}
      (response-for
       (fn [request]
         (when-let [response (spin/unknown-method? request)]
           response))
       (request :brew "/")
       [:status])))))

(deftest not-found-test
  (testing "a 404 response if resource is an empty map"
    (is
     (=
      {:status 404}
      (response-for
       (fn [request]
         (or
          (when-let [response (spin/unknown-method? request)]
            response)
          (case (:request-method request)
            (:head :get)
            (let [representation nil]
              (when-let [response (spin/not-found? representation)]
                response)))))
       (request :get "/")
       [:status])))))

(deftest method-not-allowed-test
  (testing "a 405 response if method not allowed"
    (is
     (=
      {:status 405}

      (response-for
       (fn [request]

         (when-let [response (spin/method-not-allowed? request #{:get})]
           response))

       (request :post "/")
       [:status])))))

(deftest get-and-head-test
  (let [h (fn [request]

            (when-let [response (spin/method-not-allowed? request #{:get})]
              response)

            (case (:request-method request)
              (:head :get)
              (let [representation
                    {::spin/content-type "text/plain"
                     ::spin/content-length (count (.getBytes "Hello World!\n"))}]

                (cond-> (spin/ok)
                  true (conj (spin/representation->response representation))
                  (not (spin/head? request)) ; when not HEAD …
                  ;; … we add the body ourselves
                  (conj {:body "Hello World!\n"})))))]

    (testing "a 200 response, with a body, in response to a GET request"
      (is
       (=
        {:status 200
         :headers {"content-length" "13"}
         :body "Hello World!\n"}

        (response-for
         h
         (request :get "/")
         [:status :body "content-length"]))))

    (testing "a 200 response, without a body, in response to a HEAD request"
      (let [response
            (response-for
             h
             (request :head "/"))]
        (is (= 200 (:status response)                     ))
        (is (= "13" (get-in response [:headers "content-length"])))
        (is (nil? (find response :body)))))))

(deftest bad-request-test
  (testing "a 400 response if bad request"
    (is
     (=
      {:status 400}

      (response-for
       (fn [request]
         (when-not (get-in request [:headers "some-required-header"])
           (spin/bad-request)))

       (request :get "/")
       [:status])))))

(deftest allow-test
  (is
   (=
    {:status 405
     :headers {"allow" "GET, HEAD, OPTIONS"}}
    (response-for

     (fn [request]
       (when-let [response (spin/method-not-allowed? request #{:get})]
         response))

     (request :post "/")
     [:status "allow"]))))

(deftest post-test
  (let [h (fn [request]
            (or
             (when-let [response (spin/method-not-allowed? request #{:post})]
               response)

             (spin/created "/new-resource")))]

    (testing "responds with 201 when new resource created"
      (is
       (=
        {:status 201
         :headers {"location" "/new-resource"}}
        (response-for
         h
         (request :post "/")
         [:status "location"]))))))

(deftest response-header-date-test
  (-> (fn [_] (spin/ok))
      spin/wrap-add-date
      (response-for (request :get "/"))
      (get-in [:headers "date"])
      util/parse-http-date
      inst? is))

(deftest options-test
  (testing "Default Allow header includes GET, HEAD and OPTIONS"
    (is
     (= {:status 200,
         :headers {"allow" "GET, HEAD, OPTIONS"}}
        (-> (fn [request]
              (case (:request-method request)
                :options (spin/options #{:get})))
            (response-for
             (request :options "/")
             [:status "allow"])))))

  (testing "Allow header reveals declared methods"
    (is
     (=
      {:status 200,
       :headers {"allow" "DELETE, OPTIONS"}}
      (-> (fn [request]
            (case (:request-method request)
              :options (spin/options #{:delete})))
          (response-for
           (request :options "/")
           [:status "allow"])))))

  (testing "Allow header includes HEAD when GET declared"
    (is (=
         {:status 200,
          :headers {"allow" "GET, HEAD, PUT, OPTIONS"}}
         (-> (fn [request]
               (case (:request-method request)
                 :options (spin/options #{:get :put})))
             (response-for
              (request :options "/")
              [:status "allow"])))))

  (testing "Content-Length set to 0 when no payload"
    (is (=
         {:status 200,
          :headers {"content-length" "0"}}
         (-> (fn [request]
               (case (:request-method request)
                 :options (spin/options #{:get :put})))
             (response-for
              (request :options "/")
              [:status "content-length"]))))))

(deftest conditional-if-modified-since-test
  (let [h (fn [request]
            (let [representation
                  {::spin/last-modified (util/parse-http-date "Tue, 24 Nov 2020 09:00:00 GMT")}]
              (or
               (when-let [response (spin/not-modified? request representation)]
                 response)
               (conj (spin/ok) (spin/representation->response representation)))))]

    (testing "Representation was modified since 8am. Let the request through."
      (is
       (=
        {:status 200
         :headers {"last-modified" "Tue, 24 Nov 2020 09:00:00 GMT"}}
        (response-for
         h
         (-> (request :get "/")
             (header "if-modified-since" "Tue, 24 Nov 2020 08:00:00 GMT"))
         [:status "last-modified"]))))

    (testing "Representation was modified at exactly 9am. Return 304."
      (is
       (=
        {:status 304}
        (response-for
         h
         (-> (request :get "/")
             (header "if-modified-since" "Tue, 24 Nov 2020 09:00:00 GMT"))
         [:status]))))

    (testing "Representation was not modified since 10am. Return 304."
      (is
       (=
        {:status 304}
        (response-for
         h
         (-> (request :get "/")
             (header "if-modified-since" "Tue, 24 Nov 2020 10:00:00 GMT"))
         [:status]))))))

(deftest conditional-if-none-match-test
  (let [h (fn [request]
            (let [representation {::spin/entity-tag "\"abc\""}]
              (or
               (when-let [response (spin/not-modified? request representation)]
                 response)
               (conj (spin/ok) (spin/representation->response representation)))))]
    (is
     (=
      {:status 200
       :headers {"etag" "\"abc\""}}
      (response-for
       h
       (-> (request :get "/")
           (header "if-none-match" "\"def\""))
       [:status "etag"])))

    (is
     (=
      {:status 304}
      (response-for
       h
       (-> (request :get "/")
           (header "if-none-match" "\"abc\", \"def\""))
       [:status])))))
