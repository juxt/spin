;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.resource-test
  (:require
   [clojure.test :refer [deftest is use-fixtures testing]]
   [juxt.spin.alpha.methods :refer [http-method]]
   [juxt.spin.alpha.resource :as r]
   [ring.mock.request :refer [request]]
   [clojure.tools.logging :as log]
   [juxt.spin.alpha.methods :as methods])
  (:import
   (java.util.logging LogManager Logger Level Handler)))

;;(remove-ns (symbol (str *ns*)))

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
  (let [*response (promise)]
    (http-method
     (reify
       r/GET
       (get-or-head [_ server resource response request respond raise]
         (respond (conj response [:body "Hello World!"]))))
     nil                                ; nil server-provider
     {}                                ; resource
     {}                                 ; response
     (request :get "/")
     (fn [r] (deliver *response r))
     (fn [_]))

    (is (= ["resource-provider satisfies resource/GET ? true"]
           (map (memfn getMessage) @*log-records*)))
    (let [response (deref *response 0 :timeout)]
      (is (= 200 (:status response)))
      (is (= "Hello World!" (:body response))))))

(deftest get-with-body-explicit-status-test
  (let [*response (promise)]
    (http-method
     (reify
       r/GET
       (get-or-head [_ server resource response request respond raise]
         (respond (merge response {:status 200
                                   :body "Hello World!"}))))
     nil                                ; nil server-provider
     nil                                ; nil resource
     {}                                 ; response
     (request :get "/")
     (fn [r] (deliver *response r))
     (fn [_]))

    (is (= ["resource-provider satisfies resource/GET ? true"]
           (map (memfn getMessage) @*log-records*)))
    (let [response (deref *response 0 :timeout)]
      (is (= 200 (:status response)))
      (is (= "Hello World!" (:body response))))))

(deftest get-with-body-explicit-not-ok-test
  (let [*response (promise)]
    (http-method
     (reify
       r/GET
       (get-or-head [_ server resource response request respond raise]
         (respond (merge response {:status 400
                                   :body "Bad request!"}))))
     nil                                ; nil server-provider
     nil                                ; nil resource
     {}                                 ; response
     (request :get "/")
     (fn [r] (deliver *response r))
     (fn [_]))

    (is (= ["resource-provider satisfies resource/GET ? true"]
           (map (memfn getMessage) @*log-records*)))
    (let [response (deref *response 0 :timeout)]
      (is (= 400 (:status response)))
      (is (= "Bad request!" (:body response))))))

(deftest get-with-content-response
  (let [*response (promise)]
    (http-method
     (reify
       r/ContentResponse
       (respond-with-content [_ server resource representations response request respond raise]
         (respond
          (-> response
              (update :headers (fnil conj {}) ["content-type" "text/plain;charset=utf8"])
              (conj [:body "Hello World!"])))))
     nil                                ; nil server-provider
     {}                                 ; resource
     {}                                 ; response
     (request :get "/")
     (fn [r] (deliver *response r))
     (fn [_]))

    (let [response (deref *response 0 :timeout)]
      (is (= 200 (:status response)))
      (is (= "text/plain;charset=utf8" (get-in response [:headers "content-type"])))
      (is (= "Hello World!" (:body response))))))

(deftest get-with-no-representation-404-test
  (let [*response (promise)]
    (http-method
     (reify
       r/ContentVariants
       (available-variants [_ server resource response]
         []))
     nil                                ; nil server-provider
     nil                                ; nil resource
     {}
     (request :get "/")
     (fn [r] (deliver *response r))
     (fn [_]))

    (let [response (deref *response 0 :timeout)]
      (is (= 404 (:status response))))))

(deftest get-with-no-acceptable-representation-406-test
  (let [*response (promise)]
    (http-method
     (reify
       r/ContentVariants
       (available-variants [_ server resource response]
         [:json])

       r/ContentProactiveNegotiation
       (select-representations [_ server request variants]
         []))
     nil                                ; nil server-provider
     nil                                ; nil resource
     {}                                 ; response
     (request :get "/")
     (fn [r] (deliver *response r))
     (fn [_]))

    (let [response (deref *response 0 :timeout)]
      (is (= 406 (:status response))))))

(deftest get-with-varying-content-type-test
  (let [rp (reify
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
               (log/trace "response is" (pr-str response))
               (is (= 1 (count representations)))
               (is (= {:juxt.http/content-type "text/html;charset=utf8"} (first representations)))
               (respond
                (-> response
                    (update :headers (fnil conj {}) ["content-type" (:juxt.http/content-type (first representations))])
                    (conj [:body (case (:juxt.http/content-type (first representations))
                                   "text/html;charset=utf8" "<h1>Hello World!</h1>"
                                   "application/json" "{\"message\": \"Hello World!\"}")])))))]
    (testing "GET"
      (let [*response (promise)]
        (http-method
         rp
         nil                            ; nil server-provider
         {}                             ; resource
         {}                             ; response
         (request :get "/")
         (fn [r] (deliver *response r))
         (fn [_]))

        (let [response (deref *response 0 :timeout)]
          (is (= 200 (:status response)))
          (is (= "<h1>Hello World!</h1>" (:body response)))
          (is (= "text/html;charset=utf8" (get-in response [:headers "content-type"])))
          (is (= "accept" (get-in response [:headers "vary"]))))))

    (testing "HEAD"
      (let [*response (promise)]
        (http-method
         rp
         nil                            ; nil server-provider
         {}                             ; resource
         {}                             ; response
         (request :head "/")
         (fn [r] (deliver *response r))
         (fn [_]))

        (let [response (deref *response 0 :timeout)]
          (is (= 200 (:status response)))
          (is (nil? (:body response)))
          (is (= "text/html;charset=utf8" (get-in response [:headers "content-type"])))
          (is (= "accept" (get-in response [:headers "vary"]))))))))


;; TODO: Try with a 404 content response
;; TODO: Try with a 406 content response
;; TODO: Try with a 500 content response
