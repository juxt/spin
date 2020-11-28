;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.spin.alpha :as spin]
   [juxt.spin.alpha.util :as util]
   [juxt.spin.alpha.test-util :refer [response-for request header]]))

(deftest unknown-method-test
  (testing "responds with 501 for unknown method"
    (is
     (=
      {:ring.response/status 501}
      (response-for
       {}
       (request :brew "/")
       [:ring.response/status])))))

(deftest get-request-test
  (testing "responds with 404 if resource is an empty map"
    (is
     (=
      {:ring.response/status 404}
      (response-for
       {}
       (request :get "/")
       [:ring.response/status]))))

  (testing "GET on 'Hello World!'"
    (is
     (=
      {:ring.response/status 200
       :ring.response/headers {"content-length" "13"}
       :ring.response/body "Hello World!\n"}
      (response-for
       {::spin/select-representation!
        (fn [_]
          {::spin/content-type "text/plain"
           ::spin/content "Hello World!\n"})}
       (request :get "/")
       [:ring.response/status :ring.response/body "content-length"]))))

  (testing "GET on 'Hello World!' with select-representation! callback"
    (is
     (=
      {:ring.response/status 200
       :ring.response/headers {"content-length" "13"}
       :ring.response/body "Hello World!\n"}
      (response-for
       {::spin/select-representation!
        (fn [_]
          {::spin/content-type "text/plain"
           ::spin/content "Hello World!\n"})}
       (request :get "/")
       [:ring.response/status :ring.response/body "content-length"]))))

  (testing "respond with 400 on a malformed GET on resource"
    (is
     (= {:ring.response/status 400 :ring.response/body "Bad request!"}
        (response-for
         {::spin/validate-request!
          (fn [{::spin/keys [respond! response]}]
            (respond! (assoc response :ring.response/body "Bad request!")))}
         (request :get "/")
         [:ring.response/status :ring.response/body])))))

(deftest head-request-test
  (testing "HEAD on 'Hello World!'"
    (is
     (=
      {:ring.response/status 200
       :ring.response/headers {"content-length" "13"}}
      (response-for
       {::spin/select-representation!
        (fn [_]
          {::spin/content-type "text/plain"
           ::spin/content "Hello World!\n"})}

       (request :head "/")
       [:ring.response/status :ring.response/body "content-length"]))))

  (testing "HEAD on Hello World! with representation respond!"
    (is
     (=
      {:ring.response/status 200
       :ring.response/headers {"content-length" "13"}}
      (response-for
       {::spin/select-representation!
        (fn [_]
          {::spin/content-type "text/plain"
           ::spin/content "Hello World!\n"
           ::spin/respond!
           (fn [{::spin/keys [respond! response]}]
             (respond! response))})}
       (request :head "/")
       [:ring.response/status :ring.response/body "content-length"])))))

(deftest allow-test
  (is
   (=
    {:ring.response/status 405
     :ring.response/headers {"allow" "GET, HEAD, OPTIONS"}}
    (response-for
     {}
     (request :post "/")
     [:ring.response/status "allow"]))))

(deftest post-request-test
  (testing "responds with 405 (Method Not Allowed) if POST but no post callback"
    (is
     (=
      {:ring.response/status 405}
      (response-for
       {}
       (request :post "/")
       [:ring.response/status]))))

  (testing "responds with 201 when new resource created"
    (is
     (=
      {:ring.response/status 201
       :ring.response/headers {"location" "/new-resource"}}
      (response-for
       {::spin/methods
        {:post
         (fn [ctx]
           ;; A real implementation would do some processing here.
           (spin/created! ctx "/new-resource"))}}
       (request :post "/")
       [:ring.response/status "location"])))))

;; TODO:
;; Expose last-modified and etag as response headers - test for this
;; Put with body, set representation
;; Test If-Match to avoid conflicts, produce 409

#_(response-for
 {::spin/representation
  {::spin/content "Hello World!"
   ::spin/entity-tag "\"abc\""}
  ::spin/methods
  {:get spin/GET
   :put
   (fn [ctx]
     ;; A real implementation would do some processing here.
     (spin/created! ctx "/new-resource"))}}
 (request :get "/"))

#_(response-for
 {::spin/select-representation!
  (fn [{::spin/keys [respond!]}]
    {::spin/content "foo"
     ::spin/last-modified (util/parse-http-date "Sat, 28 Nov 2020 00:57:49 GMT")})
  }
 (->
  (request :put "/")
  (header "if-modified-since" "Sat, 28 Nov 2020 00:57:49 GMT")
  ))

(deftest response-header-date-test
  (-> {::spin/representation
       {::spin/content "Hello World!\n"}}
      (response-for (request :get "/") [:ring.response/status "date"])
      (get-in [:ring.response/headers "date"])
      util/parse-http-date inst? is))

(deftest response-error-test
  (is
   (=
    {:ring.response/status 500,
     :ring.response/headers
     {"content-length" "6",
      "content-type" "text/plain;charset=utf-8"}
     :ring.response/body "ERROR!",}

    (->
     {::spin/select-representation!
      (fn [{::spin/keys [response]}]
        (case (:ring.response/status response)
          200 {::spin/respond!
               (fn [{::spin/keys [raise!]}]
                 (raise! (ex-info "Error" {})))}
          500 {::spin/content-type "text/plain;charset=utf-8"
               ::spin/content "ERROR!"}

          {::spin/content-type "text/plain;charset=utf-8"
           ::spin/content "Not 500 - test should fail"}))}

     (response-for
      (request :get "/")
      [:ring.response/status :ring.response/body "content-length" "content-type"]))))

  (testing "Custom response! in representation"
    (is
     (=
      {:ring.response/status 403,
       :ring.response/body "Custom message: Something went wrong!"}
      (-> {::spin/select-representation!
           (fn [{::spin/keys [response]}]
             (case (:ring.response/status response)
               200 {::spin/respond!
                    (fn [{::spin/keys [raise!]}]
                      ;; TODO: Can we let the ring response body be set here?
                      (raise! (ex-info "Forbidden!" {:ring.response/status 403})))}
               403 {::spin/respond!
                    (fn [{::spin/keys [respond! response]}]
                      (assert respond!)
                      (respond!
                       (assoc
                        response
                        :ring.response/body "Custom message: Something went wrong!")))
                    ::spin/content-type "text/plain;charset=utf-8"
                    ::spin/content "Error"}))}
          (response-for
           (request :get "/")
           [:ring.response/status :ring.response/body]))))))

(deftest options-test
  (testing "Default Allow header includes GET, HEAD and OPTIONS"
    (is (= {:ring.response/status 200,
            :ring.response/headers {"allow" "GET, HEAD, OPTIONS"}}
           (-> {::spin/select-representation!
                (fn [_]
                  {::spin/content "Hello World!\n"})}
               (response-for
                (request :options "/")
                [:ring.response/status "allow"])))))

  (testing "Allow header reveals declared methods"
    (is (= {:ring.response/status 200,
            :ring.response/headers {"allow" "DELETE, OPTIONS"}}
           (-> {::spin/select-representation!
                (fn [_]
                  {::spin/content "Hello World!\n"})
                ::spin/methods
                {:delete (fn [_] (throw (ex-info "" {})))}}
               (response-for
                (request :options "/")
                [:ring.response/status "allow"])))))

  (testing "Allow header includes HEAD when GET declared"
    (is (= {:ring.response/status 200,
            :ring.response/headers {"allow" "GET, HEAD, PUT, OPTIONS"}}
           (-> {::spin/select-representation!
                (fn [_]
                  {::spin/content "Hello World!\n"})
                ::spin/methods
                {:get (fn [_] (throw (ex-info "" {})))
                 :put (fn [_] (throw (ex-info "" {})))}}
               (response-for
                (request :options "/")
                [:ring.response/status "allow"])))))

  (testing "Content-Length set to 0 when no payload"
    (is (= {:ring.response/status 200,
            :ring.response/headers {"content-length" "0"}}
           (-> {::spin/select-representation!
                (fn [_]
                  {::spin/content "Hello World!\n"})}
               (response-for
                (request :options "/")
                [:ring.response/status "content-length"])))))

  (testing "OPTIONS can be overridden with a custom implementation"
    ;; The default OPTIONS implementation doesn't support extensions. Therefore,
    ;; its implementation can be easily overridden.
    (is (= {:ring.response/status 200,
            :ring.response/headers {"allow" "GET, HEAD, OPTIONS"}
            :ring.response/body "Custom options"}
           (-> {::spin/select-representation!
                (fn [_]
                  {::spin/content "Hello World!\n"})
                ::spin/methods
                {:options
                 (fn [{::spin/keys [respond!]}]
                   (respond! {:ring.response/status 200
                              :ring.response/body "Custom options"}))}}
               (response-for
                (request :options "/")
                [:ring.response/status "allow" :ring.response/body]))))))

;; RFC 7232
(deftest conditional-get-request-test
  (let [res
        {::spin/select-representation!
         (fn [_]
           {::spin/content-type "text/plain"
            ::spin/content "Hello World!\n"
            ::spin/last-modified (util/parse-http-date "Tue, 24 Nov 2020 09:00:00 GMT")
            ::spin/respond!
            (fn [{::spin/keys [respond! response]}]
              (respond! response))})}]

    (testing "Representation was modified since 8am. Let the request through."
      (is
       (=
        {:ring.response/status 200
         :ring.response/headers {"last-modified" "Tue, 24 Nov 2020 09:00:00 GMT"}}
        (response-for
         res
         (-> (request :get "/")
             (header "if-modified-since" "Tue, 24 Nov 2020 08:00:00 GMT"))
         [:ring.response/status "last-modified"]))))

    (testing "Representation was modified at exactly 9am. Return 304."
      (is
       (=
        {:ring.response/status 304}
        (response-for
         res
         (-> (request :get "/")
             ;; No, it was modified at exactly 9am. No modifications since.
             (header "if-modified-since" "Tue, 24 Nov 2020 09:00:00 GMT"))
         [:ring.response/status]))))

    (testing "Representation was not modified since 10am. Return 304."
      (is
       (=
        {:ring.response/status 304}
        (response-for
         res
         (-> (request :get "/")
             (header "if-modified-since" "Tue, 24 Nov 2020 10:00:00 GMT"))
         [:ring.response/status])))))

  (testing "GET with etags"
    (let [res
          {::spin/select-representation!
           (fn [_]
             {::spin/content-type "text/plain"
              ::spin/content "Hello World!\n"
              ::spin/entity-tag "\"abc\""
              ::spin/respond!
              (fn [{::spin/keys [respond! response]}]
                (respond! response))})}]
      (is
       (=
        {:ring.response/status 200
         :ring.response/headers {"etag" "\"abc\""}}
        (response-for
         res
         (-> (request :get "/")
             ;; Yes, def doesn't match abc
             (header "if-none-match" "\"def\""))
         [:ring.response/status "etag"])))

      (is
       (=
        {:ring.response/status 304}
        (response-for
         res
         (-> (request :get "/")
             ;; No, there's a match, so we return 304.
             (header "if-none-match" "\"abc\", \"def\""))
         [:ring.response/status]))))))
