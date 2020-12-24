;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.spin.alpha :as spin]))

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

(deftest not-implemented-test
  (testing "501 response for not implemented method"
    (is
     (=
      {:status 501}
      (response-for
       (fn [request]
         (when-let [response (spin/not-implemented? request)]
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
          (when-let [response (spin/not-implemented? request)]
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
                    {"content-type" "text/plain"
                     "content-length" (str (count (.getBytes "Hello World!\r\n")))}]

                (cond-> (assoc (spin/ok) :headers representation)
                  (not (spin/head? request)) ; when not HEAD …
                  ;; … we add the body ourselves
                  (conj {:body "Hello World!\r\n"})))))]

    (testing "a 200 response, with a body, in response to a GET request"
      (is
       (=
        {:status 200
         :headers {"content-length" "14"}
         :body "Hello World!\r\n"}

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
        (is (= "14" (get-in response [:headers "content-length"])))
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
     :headers {"allow" "GET"}}
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
      spin/parse-http-date
      inst? is))

(deftest options-test
  (testing "Default Allow header includes GET"
    (is
     (= {:status 200,
         :headers {"allow" "GET"}}
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
       :headers {"allow" "DELETE"}}
      (-> (fn [request]
            (case (:request-method request)
              :options (spin/options #{:delete})))
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
                  {"last-modified" "Tue, 24 Nov 2020 09:00:00 GMT"}]
              (or
               (when-let [response (spin/not-modified? request representation)]
                 response)
               (assoc (spin/ok) :headers representation))))]

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
            (let [representation {"etag" "\"abc\""}]
              (or
               (when-let [response (spin/not-modified? request representation)]
                 response)
               (assoc (spin/ok) :headers representation))))]
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
