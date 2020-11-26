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
          if-modified-since (when last-modified (some-> (get-in request [:ring.response/headers "if-modified-since"]) util/parse-http-date))

          entity-tag (::spin/entity-tag representation)
          if-none-match (when entity-tag
                          (some->>
                           (get-in request [:ring.response/headers "if-none-match"])
                           util/parse-if-none-match
                           (map (comp :juxt.reap.alpha.rfc7232/opaque-tag :juxt.reap.alpha.rfc7232/entity-tag))
                           set))]
      (cond
        (and (seq if-none-match) entity-tag)
        (if-none-match entity-tag)

        (and if-modified-since last-modified)
        (not (modified-since? last-modified if-modified-since))))))

(defn- get-or-head [{::spin/keys [request resource respond! raise!] :as ctx}]
  (let [{::spin/keys [representation select-representation validate-request!]} resource
        response {:ring.response/status 200}
        ctx (into {::spin/response response} ctx)]

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
        (respond! {:ring.response/status 304})

        :else
        (let [ctx (assoc ctx ::spin/representation representation)
              response
              (let [{::spin/keys [content content-length content-type]} representation
                    content-length (or content-length (when content (count content)))]
                (cond-> response
                  content-length
                  (assoc-in [:ring.response/headers "content-length"] (str content-length))
                  content-type
                  (assoc-in [:ring.response/headers "content-type"] content-type)
                  (and (= (:ring.request/method request) :get) content)
                  (assoc :ring.response/body content)))

              ctx (assoc ctx ::spin/response response)]

          (case (:ring.request/method request)
            :get
            (if-let [rep-respond! (::spin/respond! representation)]
              (rep-respond! ctx)
              (respond! response))
            :head
            (respond! response))))

      ;; If not representation, respond with 404
      (respond! {:ring.response/status 404}))))

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
    (respond! {:ring.response/status 405})))

(defn resource-created! [{::spin/keys [respond! response]} location]
  (respond!
   (into
    {:ring.response/status 201}
    (update response :ring.response/headers assoc "location" location))))

(defmethod http-method ::default [{::spin/keys [respond!]}]
  (respond! {:ring.response/status 501}))

(defn validate-request!
  "Validate the request is appropriate for the resource."
  [{::spin/keys [resource] :as ctx}]
  (let [{::spin/keys [validate-request!]} resource
        ctx (if validate-request!
              (validate-request!
               ;; If we assoc a 400 into response, there's less chance of the
               ;; user accidentally sending through a 200.
               (assoc ctx ::spin/response {:ring.response/status 400}))
              ctx)]
    (when ctx (http-method ctx))))

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
        (validate-request! ctx))))

(s/fdef locate-resource!
  :args (s/cat :ctx (s/keys :req []
                            :opt [::spin/locate-resource!
                                  ::spin/resource]))
  ;; A nil means the locate-resource chooses to respond itself
  :ret (s/nilable ::spin/resource))

(defn wrap-catch-exception [h ctx]
  ;; TODO: ctx is the 'initial context'. We should try to capture the resource,
  ;; if it's in scope. We may be able to wrap the `raise!` callback when a
  ;; resource is located, such that it is available to this wrapper.
  (fn [req respond raise]
    ;; This is duplicate logic to what happens in handler - we should maybe
    ;; construct middleware around ctx passing?
    (let [ctx (conj ctx {::spin/request req
                         ::spin/respond! respond
                         ::spin/raise! raise})]
      (h
       req respond
       (fn [e]
         (let [data (ex-data e)
               response (into {:ring.response/status 500} data)]

           (if-let [error-representation (::spin/error-representation ctx)]
             (try
               (let [representation (error-representation (dissoc ctx ::spin/respond! ::spin/raise))
                     custom-respond! (::spin/respond! representation)]

                 (if custom-respond!
                   ;; Let the representation handle the response, including
                   ;; setting all the response header fields correctly.
                   ;; TODO: Shouldn't we help it a little, at least with content-length?
                   (custom-respond! (assoc ctx ::spin/response response))

                   ;; TODO: Try to format the content according to the representation
                   ;; metadata. If it's a string, or byte[], then assume it's
                   ;; encoded correctly and respond with that. Otherwise, if it's
                   ;; a Clojure structure, convert to a string under the
                   ;; representation metadata.
                   (let [{::spin/keys [content content-length content-type respond!]} representation
                         content-length (or content-length (when content (count content)))
                         response
                         (cond-> response
                           content-length
                           (assoc-in [:ring.response/headers "content-length"] (str content-length))
                           content-type
                           (assoc-in [:ring.response/headers "content-type"] content-type)
                           content
                           (assoc :ring.response/body content))]
                     (respond response))))

               (catch Exception e
                 ;; Recover and use a plain-text representation

                 (respond
                  (into {:ring.response/headers {"content-type" "text/plain;charset=utf-8"}
                         :ring.response/body (str "Failure when selecting representation: " (pr-str e))}
                        response
                        ))))

             (into
              {:ring.response/headers {"content-type" "text/plain;charset=utf-8"}
               :ring.response/body (pr-str e)}
              response)

             )))))))

(defn wrap-date [h]
  (fn [req respond raise]
    (h req (fn [response]
             (let [status (get response :ring.response/status 200)
                   inst (java.util.Date.)]
               (respond
                (cond-> response
                  ;; While Section 7.1.1.2 of RFC 7232 states: "An origin server
                  ;; MAY send a Date header field if the response is in the 1xx
                  ;; (Informational) or 5xx (Server Error) class of status
                  ;; codes.", we choose not to, as it cannot be used for
                  ;; cacheing.
                  (and (>= status 200) (< status 500))
                  (assoc-in
                   [:ring.response/headers "date"]
                   (util/format-http-date inst))))))
       raise)))

(defn wrap-server [h]
  (fn [req respond raise]
    (h req (fn [response]
             (respond (assoc-in response [:ring.response/headers "server"] "Spin")))
       raise)))

(defn handler [ctx]
  (-> (fn [request respond! raise!]
        (locate-resource!
         (conj ctx {::spin/request request
                    ::spin/respond! respond!
                    ::spin/raise! raise!})))
      (wrap-catch-exception ctx)
      wrap-date
      wrap-server
      sync-adapt))
