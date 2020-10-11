;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.methods-test
  (:require
   [clojure.test :refer [deftest is use-fixtures are testing]]
   [clojure.java.io :as io]
   [juxt.spin.alpha.methods :refer [http-method]]
   [juxt.spin.alpha.resource :as r]
   [juxt.spin.alpha.server :as s]
   [ring.mock.request :refer [request]]
   [clojure.tools.logging :as log]
   [clojure.tools.logging.impl :as impl]
   [juxt.spin.alpha.resource :as resource])
  (:import
   (java.util.logging LogManager Logger Level Handler)))

(def ^:dynamic  *log-records* nil)

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

(def NIL_SERVER_PROVIDER nil)
(def NIL_RESOURCE nil)

(deftest get-with-body-default-status-test
  (let [*response (promise)]
    (http-method
     (reify
       r/GET
       (get-or-head [_ server resource request respond raise]
         (respond {:body "Hello World!"})))
     NIL_SERVER_PROVIDER
     NIL_RESOURCE
     {}                                 ; response
     (request :get "/")
     (fn [r] (deliver *response r))
     (fn [_]))

    (is (= ["resource-provider satisfies? resource/GET"]
           (map (memfn getMessage) @*log-records*)))
    (let [response (deref *response 0 :timeout)]
      (is (= 200 (:status response)))
      (is (= "Hello World!" (:body response))))))

(deftest get-with-body-explicit-status-test
  (let [*response (promise)]
    (http-method
     (reify
       r/GET
       (get-or-head [_ server resource request respond raise]
         (respond {:staus 200
                   :body "Hello World!"})))
     NIL_SERVER_PROVIDER
     NIL_RESOURCE
     {}                                 ; response
     (request :get "/")
     (fn [r] (deliver *response r))
     (fn [_]))

    (is (= ["resource-provider satisfies? resource/GET"]
           (map (memfn getMessage) @*log-records*)))
    (let [response (deref *response 0 :timeout)]
      (is (= 200 (:status response)))
      (is (= "Hello World!" (:body response))))))

(deftest get-with-body-explicit-not-ok-test
  (let [*response (promise)]
    (http-method
     (reify
       r/GET
       (get-or-head [_ server resource request respond raise]
         (respond {:status 400
                   :body "Bad request!"})))
     NIL_SERVER_PROVIDER
     NIL_RESOURCE
     {}                                 ; response
     (request :get "/")
     (fn [r] (deliver *response r))
     (fn [_]))

    (is (= ["resource-provider satisfies? resource/GET"]
           (map (memfn getMessage) @*log-records*)))
    (let [response (deref *response 0 :timeout)]
      (is (= 400 (:status response)))
      (is (= "Bad request!" (:body response))))))
