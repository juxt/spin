;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.readme-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.spin.alpha :as spin]
   [juxt.spin.alpha.test-util :refer [response-for request header]]))

(def hello-example
  {::spin/representation
   {::spin/content "Hello World!\n"}})

(deftest hello-example-test
  (is
   (= {:ring.response/status 200
       :ring.response/body "Hello World!\n"}
      (response-for
       hello-example
       (request :get "/")
       [:ring.response/status :ring.response/body]))))

(def bad-request-example
  {::spin/representation {}
   ::spin/validate-request!
   (fn [{::spin/keys [request respond! response] :as ctx}]
     (if (:ring.request/query request)
       ctx
       ;; No query string, bad request!
       (respond!
        (assoc
         response
         :ring.response/status 400
         :ring.response/body "Bad request!"))))})

(deftest bad-request-example-test
  (testing "respond with 400 on a malformed GET on resource"
    (is
     (= {:ring.response/status 400}
        (response-for
         bad-request-example
         (request :get "/")
         [:ring.response/status])))

    (is
     (= {:ring.response/status 200}
        (response-for
         bad-request-example
         (request :get "/?account=1234")
         [:ring.response/status])))))

(def authorization-example
  {:roles {:superuser #{:get :head :put}
           :manager #{:get :head}}
   ::spin/representation {::spin/content "Secret stuff!"}
   ::spin/validate-request!
   (fn [{::spin/keys [request respond! response resource] :as ctx}]
     (when-let [role
                (case (get-in request
                              [:ring.request/headers "authorization"])

                  "Terrible let-me-in;role=superuser"
                  :superuser

                  "Terrible let-me-in;role=manager"
                  :manager

                  (respond!
                   (-> response
                       (assoc :ring.response/status 401)
                       (assoc-in
                        [:ring.response/headers "www-authenticate"]
                        "Terrible"))))]

       (if (get-in resource [:roles role (:ring.request/method request)])
         (assoc ctx :role role)
         (respond! (assoc response :ring.response/status 403)))))})

(deftest authorization-example-test
  (testing "unauthenticated request requires authentication"
    (is
     (= {:ring.response/status 401
         :ring.response/headers {"www-authenticate" "Terrible"}}
        (response-for
         authorization-example
         (request :put "/")
         [:ring.response/status "www-authenticate"]))))

  (testing "a manager is allowed to GET"
    (is
     (= {:ring.response/status 200
         :ring.response/headers {}
         :ring.response/body "Secret stuff!"}
        (response-for
         authorization-example
         (->
          (request :get "/")
          (header "authorization" "Terrible let-me-in;role=manager"))
         [:ring.response/status "www-authenticate" :ring.response/body]))))

  (testing "a manager is not allowed to PUT, and shouldn't even know that there is no such method on this resource"
    (is
     (= {:ring.response/status 403
         :ring.response/headers {}}
        (response-for
         authorization-example
         (->
          (request :put "/")
          (header "authorization" "Terrible let-me-in;role=manager"))
         [:ring.response/status "www-authenticate"]))))

  (testing "a superuser is allowed to GET"
    (is
     (= {:ring.response/status 200
         :ring.response/headers {}
         :ring.response/body "Secret stuff!"}
        (response-for
         authorization-example
         (->
          (request :get "/")
          (header "authorization" "Terrible let-me-in;role=superuser"))
         [:ring.response/status "www-authenticate" :ring.response/body]))))

  (testing "a superuser would be allowed to PUT, if there were a PUT method defined on this resource"
    (is
     (= {:ring.response/status 405
         :ring.response/headers {}}
        (response-for
         authorization-example
         (->
          (request :put "/")
          (header "authorization" "Terrible let-me-in;role=superuser"))
         [:ring.response/status "www-authenticate"]))))

  (testing "even a superuser cannot DELETE"
    (is
     (= {:ring.response/status 403
         :ring.response/headers {}}
        (response-for
         authorization-example
         (->
          (request :delete "/")
          (header "authorization" "Terrible let-me-in;role=superuser"))
         [:ring.response/status "www-authenticate"])))))
