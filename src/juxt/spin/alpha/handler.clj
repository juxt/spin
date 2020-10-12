;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.handler
  (:require
   [clojure.string :as str]
   [juxt.spin.alpha.methods :as methods]
   [juxt.spin.alpha.resource :as resource]
   [juxt.spin.alpha.server :as server]
   [juxt.spin.alpha.conditional :refer [wrap-precondition-evalution]]
   [juxt.spin.alpha.ring :as ring]))

(defn request-url
  "Return the full URL of the request. Copied from Ring core 1.8.0, to avoid
  adding a dependency on Ring."
  [request]
  (str (-> request :scheme name)
       "://"
       (get-in request [:headers "host"])
       (:uri request)
       (when-let [query (:query-string request)]
         (str "?" query))))

(defn effective-uri [request]
  (request-url request))

(defn wrap-lookup-resource [h resource-provider]
  (fn [request respond raise]
    (try
      (if (satisfies? resource/ResourceLocator resource-provider)
        ;; Continue the chain, but with the resource assoc'd, even if nil (we might be doing a PUT, or a custom 404)
        (let [resource (resource/locate-resource resource-provider (effective-uri request) request)]
          (h (cond-> request
               resource (conj {:juxt.http/resource resource}))
             respond raise))

        ;; The will be no assoc'd resource on the request, we continue and let
        ;; the resource-provider determine the response. It is unlikely, outside of
        ;; testing and simple demos, that a resource-provider will not satisfy
        ;; http/ResourceLocator
        (h request respond raise))
      (catch Exception e
        (raise e)))))

(defn wrap-server-options [h server]
  (fn [request respond raise]
    (if (= (:uri request) "*")
      ;; Test me with:
      ;; curl -i --request-target "*" -X OPTIONS http://localhost:8000
      (respond
       (cond-> {:status 200}
         (satisfies? server/ServerOptions server) (assoc :headers (server/server-options server))))
      (h
       request
       (fn [response]
         (try
           (assert response)
           (let [server
                 (when (satisfies? server/ServerOptions server)
                   (server/server-header server))]
             (respond
              (cond-> response
                server (assoc-in [:headers "server"] server))))
           (catch Throwable t
             (raise
              (ex-info
               "Error in server-header function"
               {}
               t)))))
       raise))))

(defn- invoke-method [resource-provider server known-methods]
  (fn [request respond raise]
    (let [resource (:juxt.http/resource request)
          method (:request-method request)]

      (if (contains? known-methods method)

        (let [allowed-methods
              (if (satisfies? resource/AllowedMethods resource-provider)
                (resource/allowed-methods resource-provider server resource request)
                (or
                 (:juxt.http/allowed-methods resource) ; TODO: document this
                 #{:get}))
              response {}]

          (if (contains? allowed-methods (:request-method request))
            (try
              (methods/http-method resource-provider server resource response request respond raise)
              (catch Throwable t
                (raise
                 (ex-info
                  (format
                   "Error on %s of %s"
                   (str/upper-case (name method))
                   (:uri request))
                  {}
                  t))))
            (methods/respond-with-content-maybe
             resource-provider server resource
             nil ; representations
             (-> response (assoc :status 405)
                 (assoc-in [:headers "allow"] (str/join ", " (map str/upper-case (map name allowed-methods)))))
             request respond raise)))

        #_(let [allow (or
                       (:juxt.http/methods resource)
                       #{:get :options})
                allow (cond-> allow
                        (contains? allow :get) (conj :head)
                        (satisfies? resource/OPTIONS resource-provider) (conj :options))]
            #_(if-not (contains? allow method)
                ;; Method Not Allowed!
                (respond (cond-> {:status 405}
                           allow (conj [:headers
                                        {"allow" (str/join ", " (map (comp str/upper-case name) allow))}])))
                ;; Proceed to invoke method...
                (let [response {:headers {"allow" (str/join ", " (map (comp str/upper-case name) allow))}}]
                  (try
                    (http/http-method resource-provider server resource response request respond raise)
                    (catch Throwable t
                      (raise
                       (ex-info
                        (format
                         "Error on %s of %s"
                         (str/upper-case (name method))
                         (:uri request))
                        {:request request}
                        t)))))))

        ;; Method Not Implemented!
        ;; TODO: Content Negotiation of 501?
        (methods/respond-with-content-maybe
         resource-provider
         server resource
         nil ; representations
         {:status 501}
         request respond raise)))))


(defn handler [resource-provider server]
  (let [known-methods (set (keys (methods methods/http-method)))]
    (->
     (invoke-method resource-provider server known-methods)
     (wrap-precondition-evalution resource-provider)
     (wrap-lookup-resource resource-provider)
     (wrap-server-options server)
     ring/sync-adapt)))
