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
  [{::keys [methods]}]
  (->>
   (if (and
        methods
        (seq (dissoc methods :get :head :options)))
     (concat (keys methods) [:options])
     [:get :options])
   ;; if GET is included, so is HEAD
   (mapcat (fn [method] (if (= :get method) [:get :head] [method])))
   distinct
   (map (comp str/upper-case name))
   (str/join ", ")))

(defn method-not-allowed
  "Respond with a 405 to indicated that the resource does not support the method
  in the request."
  [request respond! _]
  (respond!
   {:ring.response/status 405
    :ring.response/headers
    {"allow" (allow-header request)}}))

(defmulti http-method
  (fn [request _ _] (:ring.request/method request))
  :default ::default)

(defmethod http-method ::default [_ respond! _]
  (respond! {:ring.response/status 501}))

(defn GET [request respond! raise!]
  (let [{::keys [select-representation!]} request
        request (assoc request :ring.response/status 200)]

    ;; This is a 'when' not an 'if'. It does not need an else clause, since the
    ;; case where representation is nil (if representation is nil and
    ;; select-representation! returns nil), the respond! callback is called to
    ;; elicit a 404 response.
    (when-let [representation
               (or
                (when select-representation!
                  (try
                    (select-representation! request respond! raise!)
                    (catch Exception e
                      (raise! (ex-info "Failed to select a representation" {:request request} e)))))
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
        (let [request (assoc request ::representation representation)
              request
              (let [{::keys [content
                             content-type content-encoding
                             content-language content-location
                             content-length content-range
                             last-modified entity-tag]}
                    representation
                    content-length
                    (or content-length (when content (count content)))]

                (cond-> request

                  content-type
                  (assoc-in
                   [:ring.response/headers "content-type"]
                   content-type)

                  content-encoding
                  (assoc-in
                   [:ring.response/headers "content-encoding"]
                   content-encoding)

                  content-language
                  (assoc-in
                   [:ring.response/headers "content-language"]
                   content-language)

                  content-location
                  (assoc-in
                   [:ring.response/headers "content-location"]
                   content-location)

                  content-length
                  (assoc-in
                   [:ring.response/headers "content-length"]
                   (str content-length))

                  content-range
                  (assoc-in
                   [:ring.response/headers "content-range"]
                   content-range)

                  last-modified
                  (assoc-in
                   [:ring.response/headers "last-modified"]
                   (util/format-http-date last-modified))

                  entity-tag
                  (assoc-in [:ring.response/headers "etag"] entity-tag)

                  (and (= (:ring.request/method request) :get) content)
                  (assoc :ring.response/body content)))]

          (case (:ring.request/method request)
            :get
            (if-let [rep-respond! (::respond! representation)]
              (rep-respond! request respond! raise!)
              (respond! request))
            :head
            (respond! request)))))))

(defmethod http-method :get [{::keys [resource] :as request} respond! raise!]
  (if (::methods resource)
    (if-let [get! (get-in resource [::methods :get])]
      (get! request respond! raise!)
      (method-not-allowed request respond! raise!))
    (GET request respond! raise!)))

(defn common-method [request respond! raise!]
  (if-let [method! (get-in request [::methods (:ring.request/method request)])]
    (method! request respond! raise!)
    (method-not-allowed request respond! raise!)))

;; This is NOT a typo, a HEAD purposely calls into the GET method
(defmethod http-method :head [request respond! raise!]
  (GET request respond! raise!))

(defmethod http-method :post [request respond! raise!]
  (common-method request respond! raise!))

(defmethod http-method :put [request respond! raise!]
  (common-method request respond! raise!))

(defmethod http-method :delete [request respond! raise!]
  (common-method request respond! raise!))

(defmethod http-method :connect [request respond! raise!]
  (method-not-allowed request respond! raise!))

(defmethod http-method :options [request respond! _]
  (let [allow (allow-header request)]
    (if-let [method! (get-in request [::methods :options])]
      (method!
       (assoc
        request
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
        {"allow" allow
         "content-length" "0"}}))))

(defmethod http-method :trace [request respond! raise!]
  (method-not-allowed request respond! raise!))

(defn validate-request!
  "Validate the request is appropriate for the resource."
  [request respond! raise!]
  (let [{::keys [validate-request!]} request
        request (if validate-request!
                  (validate-request!
                   ;; If we assoc a status of 400, there's less chance of the
                   ;; user accidentally sending through a 200.
                   (assoc request :ring.response/status 400)
                   respond!
                   raise!)
                  request)]
    (when request (http-method request respond! raise!))))

(defn add-date!
  "Compute and add a Date header to the response."
  [request respond! raise!]
  (validate-request!
   request
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
           (util/format-http-date inst))))))
   raise!))

(defn handle-errors!
  "Process request but catch and deal with any errors that may occur."
  [request respond! raise!]
  (let [raise!
        (fn [e]
          (let [data (ex-data e)
                request (merge request {:ring.response/status 500} data)]

            (if-let [select-representation! (::select-representation! request)]
              (try
                (let [representation
                      (try
                        (select-representation!
                         request respond! raise!)
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
                    (custom-respond! request respond! raise!)

                    ;; TODO: Try to format the content according to the representation
                    ;; metadata. If it's a string, or byte[], then assume it's
                    ;; encoded correctly and respond with that. Otherwise, if it's
                    ;; a Clojure structure, convert to a string under the
                    ;; representation metadata.
                    (let [{::keys [content content-length content-type]} representation
                          content-length (or content-length (when content (count content)))
                          request
                          (cond-> request
                            content-length
                            (assoc-in [:ring.response/headers "content-length"] (str content-length))
                            content-type
                            (assoc-in [:ring.response/headers "content-type"] content-type)
                            content
                            (assoc :ring.response/body content))]
                      (assert respond!)
                      (respond! request))))

                (catch Exception e
                  ;; Recover and use a plain-text representation

                  (respond!
                   (into {:ring.response/headers {"content-type" "text/plain;charset=utf-8"}
                          :ring.response/body (str "Failure when selecting representation: " (pr-str e))}
                         request))))
              ;; otherwise, if no select-representation!
              (respond!
               (into
                {:ring.response/headers {"content-type" "text/plain;charset=utf-8"}
                 :ring.response/body (pr-str e)}
                request)))))]
    (try
      (add-date! request respond! raise!)
      ;; Also, catch any exceptions that are thrown by this calling thread which
      ;; slip through uncaught.
      (catch Exception e
        (prn e)
        (raise! e)))))

(defn handler
  "Return a Ring 2.0 handler that allows requests to interact with the given
  resource. The handler suppports asynchronous and synchronous (via the
  sync-adapt wrapper) forms."
  [resource]
  (->
   (fn [request respond! raise!]
     (handle-errors!
      (conj request resource)
      respond! raise!))
   util/sync-adapt))

(defn created!
  "Convenience function for returning a 201 with a Location header."
  [location request respond! _]
  (respond!
   (-> request
       (assoc :ring.response/status 201)
       (update :ring.response/headers assoc "location" location))))
