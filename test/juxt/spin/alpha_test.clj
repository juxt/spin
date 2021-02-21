;; Copyright © 2020-2021, JUXT LTD.

(ns juxt.spin.alpha-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.spin.alpha :as spin]))

(alias 'http (create-ns 'juxt.http.alpha))

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
   (try
     (h request)
     (catch clojure.lang.ExceptionInfo e
       (if-let [exdata (ex-data e)]
         (::spin/response exdata)
         (throw e)))))
  ([h request keyseq]
   (let [keyseq (cond-> keyseq (seq (filter string? keyseq)) (conj :headers))]
     (cond-> (response-for h request)
       true
       (select-keys (filter keyword? keyseq))
       (seq (filter string? keyseq))
       (update :headers select-keys (filter string? keyseq))))))

(deftest check-not-implemented-test
  (testing "501 response for not implemented method"
    (is
     (=
      {:status 501}
      (response-for
       (fn [request]
         (spin/check-method-not-implemented! request))
       (request :brew "/")
       [:status])))))

(deftest check-not-found-test
  (testing "a 404 response if resource is an empty map"
    (is
     (=
      {:status 404}
      (response-for
       (fn [request]
         (or
          (spin/check-method-not-implemented! request)
          (case (:request-method request)
            (:head :get)
            (let [representation nil]
              (spin/check-not-found! representation)))))
       (request :get "/")
       [:status])))))

(deftest check-method-not-allowed-test
  (testing "a 405 response if method not allowed"
    (let [resource {::http/methods #{:get}}]
      (is
       (=
        {:status 405 :headers {"allow" "GET"}}
        (response-for
         (fn [request]
           (spin/check-method-not-allowed! request resource))
         (request :post "/")
         [:status "allow"]))))))

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

(deftest post-test
  (let [resource {::http/methods #{:post}}
        h (fn [request]
            (or
             (spin/check-method-not-allowed! request resource)
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
