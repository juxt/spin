;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha
  (:require
   [clojure.string :as str]
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
  (when (not (#{:connect :options :trace} (:request-method request)))
    (let [last-modified (::spin/last-modified representation)

          if-modified-since
          (when last-modified
            (some-> (get-in request [:headers "if-modified-since"])
                    util/parse-http-date))

          entity-tag (::spin/entity-tag representation)

          if-none-match
          (when entity-tag
            (some->>
             (get-in request [:headers "if-none-match"])
             util/parse-if-none-match
             (map (comp :juxt.reap.alpha.rfc7232/opaque-tag :juxt.reap.alpha.rfc7232/entity-tag))
             set))]
      (cond
        (and (seq if-none-match) entity-tag)
        (when (contains? if-none-match entity-tag)
          {:status 304})

        (and if-modified-since last-modified)
        (when-not (.isAfter (.toInstant last-modified) (.toInstant if-modified-since))
          ;; TODO: Need to distinguish which
          {:status 304})))))

(defn unknown-method?
  "When the request method is unknown, return a 501 response."
  [request]
  (when-not
      (contains?
       #{:get :put :post :delete :options :trace :connect}
       (:request-method request))
      {:status 501}))

(defn ok []
  {:status 200})

(defn not-found?
  "When representation is nil, return a 404 response."
  [representation]
  (when-not representation
    {:status 404}))

(defn allow-header
  "Return the Allow response header value, given a set of method keywords."
  [methods]
  (->>
   (if (and
        methods
        (seq (disj methods :get :head :options)))
     (concat methods [:options])
     [:get :options])
   ;; if GET is included, so is HEAD
   (mapcat (fn [method] (if (= :get method) [:get :head] [method])))
   distinct
   (map (comp str/upper-case name))
   (str/join ", ")))

(defn method-not-allowed?
  [request methods]
  (when-not (contains? methods (:request-method request))
    {:status 405
     :headers
     {"allow" (allow-header methods)}}))

(defn head? [request]
  (= (:request-method request) :head))

(defn bad-request []
  {:status 400})

(defn representation->response [representation]
  (let [{::spin/keys [content-type content-encoding
                      content-language content-location
                      content-length content-range
                      last-modified entity-tag]}
        representation]

    (cond-> {}

      content-type
      (assoc-in
       [:headers "content-type"]
       content-type)

      content-encoding
      (assoc-in
       [:headers "content-encoding"]
       content-encoding)

      content-language
      (assoc-in
       [:headers "content-language"]
       content-language)

      content-location
      (assoc-in
       [:headers "content-location"]
       content-location)

      content-length
      (assoc-in
       [:headers "content-length"]
       (str content-length))

      content-range
      (assoc-in
       [:headers "content-range"]
       content-range)

      last-modified
      (assoc-in
       [:headers "last-modified"]
       (util/format-http-date last-modified))

      entity-tag
      (assoc-in [:headers "etag"] entity-tag))))

(defn created
  "Convenience function for returning a 201 repsonse with a Location header."
  [location]
  {:status 201
   :headers {"location" location}})

(defn options
  [methods]
  {:status 200
   :headers
   {"allow" (allow-header methods)
    ;; TODO: Shouldn't this be a situation (a missing body) detected by
    ;; middleware, which can set the content-length header accordingly?
    "content-length" "0"}})

(defn wrap-add-date
  "Compute and add a Date header to the response."
  [h]
  (fn
    [request]
    (let [response (h request)]
      (let [status (get response :status 200)
            inst (java.util.Date.)]
        (cond-> response
          ;; While Section 7.1.1.2 of RFC 7232 states: "An origin server
          ;; MAY send a Date header field if the response is in the 1xx
          ;; (Informational) or 5xx (Server Error) class of status
          ;; codes.", we choose not to, as it cannot be used for
          ;; cacheing.
          (and (>= status 200) (< status 500))
          (assoc-in
           [:headers "date"]
           (util/format-http-date inst)))))))
