;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha
  (:require
   [clojure.string :as str]
   [juxt.spin.alpha.util :as util]))

(defn not-modified?
  "Return true if the given representation's validators report that it has not
  been modified with respect to the given request. This allows a 304 response to
  be returned."
  [request representation]
  ;; "… a server MUST ignore the conditional request header fields … when
  ;; received with a request method that does not involve the selection or
  ;; modification of a selected representation, such as CONNECT, OPTIONS, or
  ;; TRACE." -- Section 5, RFC 7232
  (when (not (#{:connect :options :trace} (:ring.request/method request)))
    (let [last-modified (::last-modified representation)

          if-modified-since
          (when last-modified
            (some-> (get-in request [:ring.request/headers "if-modified-since"])
                    util/parse-http-date))

          entity-tag (::entity-tag representation)

          if-none-match
          (when entity-tag
            (some->>
             (get-in request [:ring.request/headers "if-none-match"])
             util/parse-if-none-match
             (map (comp :juxt.reap.alpha.rfc7232/opaque-tag :juxt.reap.alpha.rfc7232/entity-tag))
             set))]
      (cond
        (and (seq if-none-match) entity-tag)
        (if-none-match entity-tag)

        (and if-modified-since last-modified)
        (not (.isAfter (.toInstant last-modified) (.toInstant if-modified-since)))))))

(defn allow-header
  "Produce a value for the Allow response header."
  [resource]
  (->>
   (if (and
        (::methods resource)
        (seq (dissoc (::methods resource) :get :head :options)))
     (concat (keys (::methods resource)) [:options])
     [:get :options])
   ;; if GET is included, so is HEAD
   (mapcat (fn [method] (if (= :get method) [:get :head] [method])))
   distinct
   (map (comp str/upper-case name))
   (str/join ", ")))

(defn method-not-allowed
  "Respond with a 405 to indicated that the resource does not support the method
  in the request."
  [{::keys [respond! resource]}]
  (respond!
   {:ring.response/status 405
    :ring.response/headers
    {"allow" (allow-header resource)}}))

(defmulti http-method
  (fn [{::keys [request]}] (:ring.request/method request))
  :default ::default)

(defmethod http-method ::default [{::keys [respond!]}]
  (respond! {:ring.response/status 501}))

(defn GET [{::keys [request resource respond! raise!] :as ctx}]
  (let [{::keys [representation select-representation!]} resource
        response {:ring.response/status 200}
        ctx (into {::response response} ctx)]

    ;; This is a 'when' not an 'ifi. It does not need an else clause, since
    ;; the case where representation is nil (if representation is nil and
    ;; select-representation! returns nil), the respond! callback is called to
    ;; elicit a 404 response.
    (when-let [representation
               (or
                representation
                (when select-representation!
                  (try
                    (select-representation! (dissoc ctx ::respond! ::raise))
                    (catch Exception e
                      (raise! (ex-info "Failed to select a representation" {:ctx ctx} e)))))
                (respond! {:ring.response/status 404}))]

      ;; Now to evaluate conditional requests. Note that according to Section 5,
      ;; RFC 7232, "redirects and failures take precedence over the evaluation
      ;; of preconditions in conditional requests." Therefore, we don't evaluate
      ;; whether the request is conditional until we have determined its status
      ;; code.
      (cond
        (not-modified? request representation)
        (respond! {:ring.response/status 304})

        :else
        (let [ctx (assoc ctx ::representation representation)
              response
              (let [{::keys [content content-length content-type]} representation
                    content-length (or content-length (when content (count content)))]
                (cond-> response
                  content-length
                  (assoc-in [:ring.response/headers "content-length"] (str content-length))
                  content-type
                  (assoc-in [:ring.response/headers "content-type"] content-type)
                  (and (= (:ring.request/method request) :get) content)
                  (assoc :ring.response/body content)))

              ctx (assoc ctx ::response response)]

          (case (:ring.request/method request)
            :get
            (if-let [rep-respond! (::respond! representation)]
              (rep-respond! ctx)
              (respond! response))
            :head
            (respond! response)))))))

(defmethod http-method :get [{::keys [resource] :as ctx}]
  (if (::methods resource)
    (if-let [get! (get-in resource [::methods :get])]
      (get! ctx)
      (method-not-allowed ctx))
    (GET ctx)))

(defn common-method [{::keys [request resource] :as ctx}]
  (if-let [method! (get-in resource [::methods (:ring.request/method request)])]
    (method! ctx)
    (method-not-allowed ctx)))

;; This is NOT a typo, a HEAD purposely calls into the GET method
(defmethod http-method :head [ctx] (GET ctx))
(defmethod http-method :post [ctx] (common-method ctx))
(defmethod http-method :put [ctx] (common-method ctx))
(defmethod http-method :delete [ctx] (common-method ctx))
(defmethod http-method :connect [ctx] (method-not-allowed ctx))

(defmethod http-method :options [{::keys [respond! resource] :as ctx}]
  (let [allow (allow-header resource)]
    (if-let [method! (get-in resource [::methods :options])]
      (method!
       (assoc
        ctx
        ::respond!
        ;; We add the Allow header to custom implementations so they don't have
        ;; to recompute it.
        (fn [response]
          (respond!
           (assoc-in response [:ring.response/headers "allow"] allow)))
        ;; But we allow direct access to respond! as well (TODO: document this)
        ::raw-respond! respond!))
      (respond!
       {:ring.response/status 200
        :ring.response/headers
        {"allow" (allow-header resource)
         "content-length" "0"}}))))

(defmethod http-method :trace [ctx] (method-not-allowed ctx))

(defn resource-created!
  "Convenience function for returning a 201 with a Location header."
  [{::keys [respond! response]} location]
  (respond!
   (into
    {:ring.response/status 201}
    (update response :ring.response/headers assoc "location" location))))

(defn validate-request!
  "Validate the request is appropriate for the resource."
  [{::keys [resource] :as ctx}]
  (let [{::keys [validate-request!]} resource
        ctx (if validate-request!
              (validate-request!
               ;; If we assoc a 400 into the response, there's less chance of
               ;; the user accidentally sending through a 200.
               (assoc ctx ::response {:ring.response/status 400}))
              ctx)]
    (when ctx (http-method ctx))))

(defn add-date!
  "Compute and add a Date header to the response."
  [{::keys [respond!] :as ctx}]
  (validate-request!
   (assoc
    ctx
    ::respond!
    (fn [response]
      (let [status (get response :ring.response/status 200)
            inst (java.util.Date.)]
        (respond!
         (cond-> response
           ;; While Section 7.1.1.2 of RFC 7232 states: "An origin server
           ;; MAY send a Date header field if the response is in the 1xx
           ;; (Informational) or 5xx (Server Error) class of status
           ;; codes.", we choose not to, as it cannot be used for
           ;; cacheing.
           (and (>= status 200) (< status 500))
           (assoc-in
            [:ring.response/headers "date"]
            (util/format-http-date inst)))))))))

(defn handle-errors!
  "Process request but catch and deal with any errors that may occur."
  [{::keys [respond! resource] :as ctx}]
  (let [raise!
        (fn [e]
          (let [data (ex-data e)
                response (into {:ring.response/status 500} data)]

            (if-let [select-representation! (::select-representation! resource)]
              (try
                (let [representation
                      (try
                        (select-representation!
                         (assoc ctx ::response response))
                        (catch Exception e
                          (respond! {:ring.response/status 500
                                     ;; Only in dev mode!
                                     :ring.response/body
                                     (pr-str
                                      (ex-info
                                       "Failed to select a representation for cause"
                                       {:underlying-error e}))})))
                      custom-respond! (::respond! representation)]

                  (if custom-respond!
                    ;; Let the representation handle the response, including
                    ;; setting all the response header fields correctly.
                    ;; TODO: Shouldn't we help it a little, at least with content-length?
                    (custom-respond! (assoc ctx ::response response))

                    ;; TODO: Try to format the content according to the representation
                    ;; metadata. If it's a string, or byte[], then assume it's
                    ;; encoded correctly and respond with that. Otherwise, if it's
                    ;; a Clojure structure, convert to a string under the
                    ;; representation metadata.
                    (let [{::keys [content content-length content-type]} representation
                          content-length (or content-length (when content (count content)))
                          response
                          (cond-> response
                            content-length
                            (assoc-in [:ring.response/headers "content-length"] (str content-length))
                            content-type
                            (assoc-in [:ring.response/headers "content-type"] content-type)
                            content
                            (assoc :ring.response/body content))]
                      (assert respond!)
                      (respond! response))))

                (catch Exception e
                  ;; Recover and use a plain-text representation

                  (respond!
                   (into {:ring.response/headers {"content-type" "text/plain;charset=utf-8"}
                          :ring.response/body (str "Failure when selecting representation: " (pr-str e))}
                         response))))
              ;; otherwise, if no select-representation!
              (respond!
               (into
                {:ring.response/headers {"content-type" "text/plain;charset=utf-8"}
                 :ring.response/body (pr-str e)}
                response)))))]
    (try
      (add-date!
       (assoc ctx ::raise! raise!))
      ;; Also, catch any exceptions that are thrown by this calling thread which
      ;; slip through uncaught.
      (catch Exception e
        (raise! e)))))

(defn handler
  "Return a Ring 2.0 handler that allows requests to interact with the given
  resource. The handler suppports asynchronous and synchronous (via the
  sync-adapt wrapper) forms."
  [resource]
  (->
   (fn [request respond! raise!]
     (handle-errors!
      {::resource resource
       ::request request
       ::respond! respond!
       ::raise! raise!}))
   util/sync-adapt))
