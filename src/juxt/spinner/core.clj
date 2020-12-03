;; Copyright © 2020, JUXT LTD.

(ns juxt.spinner.core
  (:require
   [juxt.spin.alpha :as spin]
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

(defn unknown-method?
  "When the request method is unknown, return a 501 response."
  [request]
  (when-not (contains? #{:get :put :post :delete :options :trace :connect} (:ring.request/method request))
    {:ring.response/status 501}))

(defn not-found?
  "When representation is nil, return a 404 response."
  [representation]
  (when-not representation
    {:ring.response/status 404}))

(defn method-not-allowed?
  "The given request parameter is required to have a ::spin/methods set containing
  method keywords."
  [request]
  (assert (::spin/methods request))
  (when-not (contains? (::spin/methods request) (:ring.request/method request))
    {:ring.response/status 405}))

(defn head? [request]
  (= (:ring.request/method request) :head))

(defn bad-request []
  {:ring.response/status 400})

(defn GET! [request representation respond! raise!]
  (assert (contains? #{:get :head} (:ring.request/method request)))

  ;; Now to evaluate conditional requests. Note that according to Section 5,
  ;; RFC 7232, "redirects and failures take precedence over the evaluation
  ;; of preconditions in conditional requests." Therefore, we don't evaluate
  ;; whether the request is conditional until we have determined its status
  ;; code.
  (cond
    (not-modified? request representation)
    (respond! {:ring.response/status 304})

    :else
    representation))

(defn representation->response [representation]
  (let [{::spin/keys [content-type content-encoding
                      content-language content-location
                      content-length content-range
                      last-modified entity-tag]}
        representation]

    (cond-> {}

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
      (assoc-in [:ring.response/headers "etag"] entity-tag))))
