;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.handler
  (:require
   [clojure.string :as str]
   [juxt.spin.alpha.methods :as methods]
   [juxt.spin.alpha.resource :as resource]
   [juxt.spin.alpha.server :as server]
   [juxt.spin.alpha.conditional :refer [wrap-precondition-evalution]]
   [juxt.spin.alpha.ring :as ring]
   [clojure.tools.logging :as log]))

(defn request-url
  "Return the full URL of the request. Copied from Ring core 1.8.0, to avoid
  adding a dependency on Ring."
  [request]
  (assert (:scheme request))
  (str (-> request :scheme name)
       "://"
       (get-in request [:headers "host"])
       (:uri request)
       (when-let [query (:query-string request)]
         (str "?" query))))

(defn effective-uri [request]
  (request-url request))

;; Continue the chain, but with the resource assoc'd, even if nil (we might be doing a PUT, or a custom 404)

(defn wrap-lookup-resource [h resource-provider server]
  (fn [request respond raise]
    (try
      (if-let [resource (resource/locate-resource resource-provider (effective-uri request) request)]
        (h (cond-> request
             resource (conj [:juxt.http/resource resource]))
           respond raise)

        ;; 404: Not found
        (let [response {:status 404}
              variants (resource/available-variants resource-provider server nil response)
              representation (first (resource/select-representations resource-provider server request variants))]

          (resource/respond-with-error resource-provider server nil representation response request respond raise)))

      (catch Exception e
        (raise e)))))

;;(h (conj request [:juxt.http/resource {}]) respond raise)

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
              (resource/allowed-methods resource-provider server resource request)
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

            ;; We have to now return a 405
            (methods/respond-with-content-maybe
             resource-provider server resource
             ;; TODO: This is just to get the tests passing, but really we
             ;; should do content-negotiation on this 405 response. This is
             ;; similar to the code that handles a 406.

             [{:juxt.http/content-type "text/plain;utf8"}]
             (-> response (assoc :status 405)
                 (assoc-in
                  [:headers "allow"]
                  (str/join ", " (map str/upper-case (map name allowed-methods)))))
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
         nil                            ; representations
         {:status 501}
         request respond raise)))))


(defn known-methods []
  (set (remove namespace (keys (methods methods/http-method)))))

(defn handler
  ([resource-provider]
   (handler resource-provider nil))
  ([resource-provider server]
   (let [resource-provider
         (resource/->ResourceProviderProxy
          resource-provider
          (resource/->BackingResourceProvider))
         known-methods (known-methods)]
     (->
      (invoke-method resource-provider server known-methods)
      ;;(wrap-precondition-evalution resource-provider)
      (wrap-lookup-resource resource-provider server)
      (wrap-server-options server)
      ring/sync-adapt))))
