;; Copyright © 2020, JUXT LTD.

(ns demo
  (:require
   [juxt.pick.alpha.ring :refer [pick]]
   [juxt.spin.alpha :as spin]
   [clojure.set :as set]
   [ring.adapter.jetty :as jetty]))

(defn locate-resource [path]
  (when-let [resource
             (case path
               "/index.html"
               {::spin/methods #{:get}})]

    (conj resource [::spin/path path])))

(defn available-representations [resource]
  (case (::spin/path resource)
    "/index.html"
    [{"content-type" "text/html;charset=utf-8"
      "content-language" "en"
      "content-location" "/en/index.html"}

     {"content-type" "text/html;charset=utf-8"
      "content-language" "de"
      "content-location" "/de/index.html"}

     {"content-type" "text/html;charset=utf-8"
      "content-language" "es"
      "content-location" "/es/index.html"}]))

(defn to-pick [{:strs [content-type content-encoding content-language] :as representation}]
  (cond-> representation
    content-type (conj [:juxt.pick.alpha/content-type content-type])
    content-encoding (conj [:juxt.pick.alpha/content-encoding content-encoding])
    content-language (conj [:juxt.pick.alpha/content-language content-language])))

(defn response-body [representation]
  (def representation representation)
  (case (get representation "content-location")
    "/en/index.html" "<h1>Welcome to the spin demo!</h1>\n"
    "/de/index.html" "<h1>Willkommen zur Spin-Demo!</h1>\n"
    "/es/index.html" "<h1>¡Bienvenida a la demo de spin!</h1>\n"))

(comment
  (locate-resource "/index.html"))

(defn handler [request]

  (try
    ;; Check method is known
    (if-let [response (spin/unknown-method? request)]
      response

      ;; Locate the resource
      (let [resource (locate-resource (:uri request))]

        ;; Check method allowed
        (if-let [response (spin/method-not-allowed? request (::spin/methods resource))]
          response

          ;; Select the representation (only if GET)
          (let [representation
                (when (#{:get :head} (:request-method request))
                  (let [available (available-representations resource)]
                    (when (empty? available)
                      (throw
                       (ex-info
                        "Not Found"
                        {::spin/error true
                         ::spin/response {:status 404}})))

                    (let [pick-result (pick
                                       request
                                       (map to-pick (available-representations {::spin/path "/index.html"})))]
                      (def pick-result pick-result)
                      (let [representation (:juxt.pick.alpha/representation pick-result)]
                        (if representation
                          representation
                          (throw
                           (ex-info "Not Acceptable" {::spin/error true
                                                      ::spin/response {:status 406}})))))))]


            {:status 200
             :headers (conj {}
                            (select-keys
                             representation ["content-type" "content-language" "content-encoding" "content-location"]))
             :body (response-body representation)}))))

    (catch clojure.lang.ExceptionInfo e
      (let [exdata (ex-data e)]
        (if (::spin/error exdata)
          (conj
           (::spin/response exdata)
           [:body (.getMessage e)]))))

    (catch Exception e
      (.printStackTrace e *out*)
      {:status 500
       :body "TODO: Print out error"}
      )))

(defonce server (jetty/run-jetty #'handler {:port 8080 :join? false}))
