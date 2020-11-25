;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.ctx-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.spec.test.alpha :as stest]
   [juxt.spin.alpha.ctx :as ctx]
   [juxt.spin.alpha :as spin]
   [juxt.spin.alpha.util :as util]))

(stest/instrument `ctx/locate-resource!)

(defn response-for
  ([ctx request]
   ((ctx/handler ctx) request))
  ([ctx request keyseq]
   (let [keyseq (cond-> keyseq (seq (filter string? keyseq)) (conj :headers))]
     (cond-> (response-for ctx request)
       true
       (select-keys (filter keyword? keyseq))
       (seq (filter string? keyseq))
       (update :headers select-keys (filter string? keyseq))))))

(defn header [request header value]
  (-> request
      (assoc-in [:headers header] value)))

(defn request [method path]
  {:ring.request/method method
   :ring.request/path path})

(deftest unknown-method-test
  (testing "responds with 501 for unknown method"
    (is
     (=
      {:status 501}
      (response-for

       {::spin/locate-resource! (fn [_] {})}

       (request :brew "/")
       [:status])))))

(deftest get-request-test
  (testing "responds with 404 if resource is an empty map"
    (is
     (=
      {:status 404}
      (response-for

       {::spin/resource {}}

       (request :get "/")
       [:status]))))

  (testing "responds with 404 if no resource or locate-resource! callback"
    ;; The resource will default to {}, which has no current representation
    (is
     (=
      {:status 404}
      (response-for

       {}

       (request :get "/")
       [:status]))))

  (testing "responds with 404 if locate-resource! returns an empty resource"
    (is
     (=
      {:status 404}
      (response-for

       {::spin/locate-resource!
        (fn [_] {})}

       (request :get "/")
       [:status]))))

  (testing "locate-resource! can respond"
    (is
     (=
      {:status 400}
      (response-for

       {::spin/locate-resource!
        (fn [{::spin/keys [respond!]}]
          (respond! {:status 400}))}

       (request :get "/")
       [:status]))))

  (testing "resource overrides locate-resource!"
    (is
     (= {:status 404}
        (response-for

         {::spin/resource {}
          ::spin/locate-resource!
          (fn [{::spin/keys [respond!]}]
            (respond!
             ;; We'll return 400 so we can distinguish
             {:status 400}))}

         (request :get "/")
         [:status]))))

  (testing "GET on 'Hello World!'"
    (is
     (=
      {:status 200
       :headers {"content-length" "13"}
       :body "Hello World!\n"}
      (response-for

       {::spin/resource
        {::spin/representation
         {::spin/content-type "text/plain"
          ::spin/content "Hello World!\n"}}}

       (request :get "/")
       [:status :body "content-length"]))))

  (testing "GET on 'Hello World!' with select-representation callback"
    (is
     (=
      {:status 200
       :headers {"content-length" "13"}
       :body "Hello World!\n"}
      (response-for

       {::spin/resource
        {::spin/select-representation
         (fn [_]
           {::spin/content-type "text/plain"
            ::spin/content "Hello World!\n"})}}

       (request :get "/")
       [:status :body "content-length"]))))

  (testing "GET on 'Hello World!' with representation respond!"
    (is
     (=
      {:status 200
       :headers {"content-length" "13"}
       :body "Hello World!\n"}
      (response-for

       {::spin/resource
        {::spin/select-representation
         (fn [_]
           {::spin/content-type "text/plain"
            ::spin/respond!
            (fn [{::spin/keys [respond! response]}]
              (respond!
               (-> response
                   (assoc :body "Hello World!\n")
                   (assoc-in [:headers "content-length"]
                             (str (count "Hello World!\n"))))))})}}

       (request :get "/")
       [:status :body "content-length"]))))

  (testing "respond with 400 on a malformed GET on resource"
    (is
     (= {:status 400 :body "Bad request!"}
        (response-for

         {::spin/resource
          {::spin/good-request!
           (fn [{::spin/keys [respond! response]}]
             (respond! (assoc response :body "Bad request!")))}}

         (request :get "/")
         [:status :body])))))

(deftest head-request-test

  (testing "HEAD on 'Hello World!'"
    (is
     (=
      {:status 200
       :headers {"content-length" "13"}}
      (response-for

       {::spin/resource
        {::spin/representation
         {::spin/content-type "text/plain"
          ::spin/content "Hello World!\n"}}}

       (request :head "/")
       [:status :body "content-length"]))))

  (testing "HEAD on 'Hello World!' with select-representation callback"
    (is
     (=
      {:status 200
       :headers {"content-length" "13"}}
      (response-for

       {::spin/resource
        {::spin/select-representation
         (fn [_]
           {::spin/content-type "text/plain"
            ::spin/content "Hello World!\n"})}}

       (request :head "/")
       [:status :body "content-length"]))))

  (testing "HEAD on Hello World! with representation respond!"
    (is
     (=
      {:status 200
       :headers {"content-length" "13"}}
      (response-for

       {::spin/resource
        {::spin/select-representation
         (fn [_]
           {::spin/content-type "text/plain"
            ::spin/content "Hello World!\n"
            ::spin/respond!
            (fn [{::spin/keys [respond! response]}]
              (respond! response))})}}

       (request :head "/")
       [:status :body "content-length"])))))

(deftest post-request-test
  (testing "responds with 405 (Method Not Allowed) if POST but no post! callback"
    (is
     (=
      {:status 405}
      (response-for

       {::spin/resource {}}

       (request :post "/")
       [:status]))))

  (testing "responds with 201 when new resource created"
    (is
     (=
      {:status 201 :headers {"location" "/new-resource"}}
      (response-for

       {::spin/resource
        {::spin/post!
         (fn [ctx]
           ;; A real implementation would do some processing here.
           (ctx/resource-created! ctx "/new-resource"))}}

       (request :post "/")
       [:status "location"])))))

(deftest conditional-get-request-test
  (let [res
        {::spin/resource
         {::spin/representation
          {::spin/content-type "text/plain"
           ::spin/content "Hello World!\n"
           ::spin/last-modified (util/parse-http-date "Tue, 24 Nov 2020 09:00:00 GMT")
           ::spin/respond!
           (fn [{::spin/keys [respond! response]}]
             (respond! response))}}}]

    (testing "Representation was modified since 8am. Let the request through."
      (is
       (=
        {:status 200}
        (response-for
         res
         (-> (request :get "/")
             (header "if-modified-since" "Tue, 24 Nov 2020 08:00:00 GMT"))
         [:status]))))

    (testing "Representation was modified at exactly 9am. Return 304."
      (is
       (=
        {:status 304}
        (response-for
         res
         (-> (request :get "/")
             ;; No, it was modified at exactly 9am. No modifications since.
             (header "if-modified-since" "Tue, 24 Nov 2020 09:00:00 GMT"))
         [:status]))))

    (testing "Representation was not modified since 10am. Return 304."
      (is
       (=
        {:status 304}
        (response-for
         res
         (-> (request :get "/")
             (header "if-modified-since" "Tue, 24 Nov 2020 10:00:00 GMT"))
         [:status])))))

  (testing "GET with etags"
    (let [res
          {::spin/resource
           {::spin/representation
            {::spin/content-type "text/plain"
             ::spin/content "Hello World!\n"
             ::spin/entity-tag "\"abc\""
             ::spin/respond!
             (fn [{::spin/keys [respond! response]}]
               (respond! response))}}}]
      (is
       (=
        {:status 200}
        (response-for
         res
         (-> (request :get "/")
             ;; Yes, def doesn't match abc
             (header "if-none-match" "\"def\""))
         [:status])))

      (is
       (=
        {:status 304}
        (response-for
         res
         (-> (request :get "/")
             ;; No, there's a match, so we return 304.
             (header "if-none-match" "\"abc\", \"def\""))
         [:status]))))))
