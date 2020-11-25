;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.ctx
  (:require
   [clojure.spec.alpha :as s]
   [juxt.spin.alpha :as spin]
   [juxt.spin.alpha.util :as util])
  (:import
   (java.util Date)))

(defn sync-adapt [h]
  (fn this
    ([req]
     (let [p (promise)]
       (this
        req
        (fn [response]
          (deliver p response)
          ;; It's crucial this returns nil, as these are semantics of the async
          ;; version
          nil)
        (fn [error]
          (deliver p error)
          nil))
       (let [res (deref p 1000 ::timeout)]
         (cond
           (= res ::timeout)
           (throw
            (ex-info
             "Timeout occured waiting for handler"
             {:handler h
              :request req}))
           (instance? Throwable res)
           (throw res)
           :else
           res))))
    ([req respond! raise!]
     (h req respond! raise!))))

(defn modified-since? [^Date this ^Date other]
  (.isAfter (.toInstant this) (.toInstant other)))

(defn precondition-failed? [request representation]
  ;; "… a server MUST ignore the conditional request header fields … when
  ;; received with a request method that does not involve the selection or
  ;; modification of a selected representation, such as CONNECT, OPTIONS, or
  ;; TRACE." -- Section 5, RFC 7232
  (when (not (#{:connect :options :trace} (:ring.request/method request)))


    (let [last-modified (::spin/last-modified representation)
          if-modified-since (when last-modified (some-> (get-in request [:headers "if-modified-since"]) util/parse-http-date))

          entity-tag (::spin/entity-tag representation)
          if-none-match (when entity-tag
                          (some->>
                           (get-in request [:headers "if-none-match"])
                           util/parse-if-none-match
                           (map (comp :juxt.reap.alpha.rfc7232/opaque-tag :juxt.reap.alpha.rfc7232/entity-tag))
                           set))]
      (cond
        (and (seq if-none-match) entity-tag)
        (if-none-match entity-tag)

        (and if-modified-since last-modified)
        (not (modified-since? last-modified if-modified-since))))))

(defn- get-or-head [{::spin/keys [request resource respond! raise!] :as ctx}]
  (let [{::spin/keys [select-representation good-request! representation]} resource
        status 200
        ctx (into {::spin/status status} ctx)]


    (when
        ;; This guard gives us the opportunity to validate that request is
        ;; appropriate for the resource, allowing us to respond directly with a 400
        ;; (or similar) and escape processing.
        (or
         (nil? good-request!)
         (good-request!
          (assoc ctx ::spin/response {:status 400})))

        (if-let [representation
                 (or
                  representation
                  (when select-representation
                    (try
                      (select-representation (dissoc ctx ::spin/respond! ::spin/raise))
                      (catch Exception e
                        (raise! (ex-info "Failed to locate-resource" {:ctx ctx} e))))))]

          ;; Now to evaluate conditional requests. Note that according to Section 5,
          ;; RFC 7232, "redirects and failures take precedence over the evaluation
          ;; of preconditions in conditional requests." Therefore, we don't evaluate
          ;; whether the request is conditional until we have determined its status
          ;; code.
          (cond
            (precondition-failed? request representation)
            (respond! {:status 304})

            :else
            (let [ctx (assoc ctx ::spin/representation representation)
                  response
                  (let [{::spin/keys [content content-length content-type]} representation
                        content-length (or content-length (when content (count content)))]
                    (cond-> {:status status}
                      content-length
                      (assoc-in [:headers "content-length"] (str content-length))
                      content-type
                      (assoc-in [:headers "content-type"] content-type)
                      (and (= (:ring.request/method request) :get) content)
                      (assoc :body content)))

                  ctx (assoc ctx ::spin/response response)]

              (case (:ring.request/method request)
                :get
                (if-let [rep-respond! (::spin/respond! representation)]
                  (rep-respond! ctx)
                  (respond! response))
                :head
                (respond! response))))

          ;; If not representation, respond with 404
          (respond! {:status 404})))))

(defmulti http-method
  (fn [{::spin/keys [request]}] (:ring.request/method request))
  :default ::default)

(defmethod http-method :get [ctx]
  (get-or-head ctx))

(defmethod http-method :head [ctx]
  (get-or-head ctx))

(defmethod http-method :post [{::spin/keys [resource respond!] :as ctx}]
  (if-let [method! (::spin/post! resource)]
    (method! ctx)
    (respond! {:status 405})))

(defn resource-created! [{::spin/keys [respond! response]} location]
  (respond!
   (into
    {:status 201}
    (update response :headers assoc "location" location))))

(defmethod http-method ::default [{::spin/keys [respond!]}]
  (respond! {:status 501}))

(defn locate-resource!
  [{::spin/keys [locate-resource! resource raise!] :as ctx}]
  (when-let
      ;; If the resource is nil, this indicates the locate-resource callback has
      ;; responded itself.
      [resource
       (cond
         resource resource
         locate-resource! (try
                            (locate-resource! ctx)
                            (catch Exception e
                              (raise! (ex-info "Failed to locate-resource" {:ctx ctx} e))))
         :else {})]

      (let [ctx (assoc ctx ::spin/resource resource)]
        (http-method ctx))))

(s/fdef locate-resource!
  :args (s/cat :ctx (s/keys :req []
                            :opt [::spin/locate-resource!
                                  ::spin/resource]))
  ;; A nil means the locate-resource chooses to respond itself
  :ret (s/nilable ::spin/resource))

(defn wrap-date [h]
  (fn [req respond raise]
    (h req (fn [response]
             (let [inst (java.util.Date.)]
               (respond
                (assoc-in
                 response
                 [:headers "date"]
                 (util/format-http-date inst)))))
       raise)))

(defn wrap-server [h]
  (fn [req respond raise]
    (h req (fn [response]
             (respond (assoc-in response [:headers "server"] "Spin")))
       raise)))

(defn handler [ctx]
  (-> (fn [request respond! raise!]
        (locate-resource!
         (conj ctx {::spin/request request
                    ::spin/respond! respond!
                    ::spin/raise! raise!})))
      wrap-date
      wrap-server
      sync-adapt))
