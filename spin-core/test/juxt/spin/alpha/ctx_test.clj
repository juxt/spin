;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.ctx-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.spec.test.alpha :as stest]
   [juxt.spin.alpha.ctx :as ctx]
   [juxt.spin.alpha :as spin]))

(stest/instrument `ctx/locate-resource)

(defn response-for
  ([ctx request]
   ((ctx/handler ctx) request))
  ([ctx request keyseq]
   (select-keys
    (response-for ctx request)
    keyseq)))

(defn request [method path]
  {:ring.request/method method
   :ring.request/path path})

(deftest response-test
  (testing "responds with 404 if resource is an empty map"
    (is
     (=
      {:status 404}
      (response-for

       #::spin{:resource {}}

       (request :get "/")))))

  (testing "responds with 404 if no resource or locate-resource callback"
    ;; The resource will default to {}, which has no current representation
    (is
     (=
      {:status 404}
      (response-for

       #::spin{}

       (request :get "/")
       [:status]))))

  (testing "responds with 404 if locate-resource returns an empty resource"
    (is
     (=
      {:status 404}
      (response-for

       #::spin{:locate-resource
               (fn [_] {})}

       (request :get "/")
       [:status]))))

  (testing "locate-resource can respond"
    (is
     (=
      {:status 400}
      (response-for

       #::spin{:locate-resource
               (fn [{::spin/keys [respond!]}]
                 (respond! {:status 400}))}

       (request :get "/")
       [:status]))))

  (testing "resource overrides locate-resource"
    (is
     (= {:status 404}
        (response-for

         #::spin{:resource {}
                 :locate-resource
                 (fn [{::spin/keys [respond!]}]
                   (respond!
                    ;; We'll return 400 so we can distinguish
                    {:status 400}))}

         (request :get "/")))))

  (testing "responds 501 for unknown method"
    (is
     (=
      {:status 501}
      (response-for

       #::spin{:locate-resource (fn [_] {})}

       (request :brew "/")
       [:status]))))

  (testing "Hello World!"
    (is
     (=
      {:status 200
       :body "Hello World\n"}
      (response-for

       #::spin
       {:resource
        #::spin
        {:get-or-head!
         (fn [{::spin/keys [status respond!]}]
           (is (= status 404))
           ;; We will return a fixed message, overriding the status
           (respond! {:status 200 :body "Hello World\n"}))}}

       (request :get "/")
       [:status :body]))))

  (testing "GET with callback"
    (is
     (=
      {:status 200
       :body "Hello World!\n"}
      (response-for

       #::spin
       {:resource
        #::spin
        {:get-or-head!
         (fn [{::spin/keys [status respond!]}]
           (is (= status 404))
           ;; We will return a fixed message, overriding the status
           (respond! {:status 200 :body "Hello World!\n"}))}}

       (request :get "/")
       [:status :body]))))

  (testing "GET with representation"
    (is
     (=
      {:status 200
       :body "Hello World!\n"}
      (response-for

       #::spin
       {:resource
        #::spin
        {:representation
         #::spin{:content "Hello World!\n"}}}

       (request :get "/")
       [:status :body]))))

  (testing "responds with 405 if post but no post! callback"
    (is
     (=
      {:status 405}
      (response-for
       #::spin{:resource {}}
       (request :post "/")
       [:status])))))
