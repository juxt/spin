;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha
  (:require
   [clojure.string :as str]
   [juxt.reap.alpha.decoders :as reap]
   [juxt.reap.alpha.rfc7231 :as rfc7231]
   [juxt.reap.alpha.rfc7232 :as rfc7232]))

(defn check-method-not-implemented!
  "When the request method is not implemented, return a 501 response."
  ([request]
   (check-method-not-implemented! request #{:get :head :put :post :delete :options :trace :connect}))
  ([request methods]
   (when-not (contains?
              methods
              (:request-method request))
     (throw
      (ex-info
       "Method not implemented"
       {::response
        {:status 501 :body "Not Implemented\r\n"}})))))

(defn ok []
  {:status 200})

(defn check-not-found!
  "When representation is nil, return a 404 response."
  [representation]
  (when-not representation
    (throw
     (ex-info
      "Not found"
      {::response
       {:status 404 :body "Not Found\r\n"}}))))

(defn allow-header
  "Return the Allow response header value, given a set of method keywords."
  [methods]
  (->>
   methods
   seq
   distinct
   (map (comp str/upper-case name))
   (str/join ", ")))

(defn check-method-not-allowed!
  [request resource]
  (if resource
    (let [methods (::methods resource)
          method (:request-method request)]
      (when-not (contains? methods method)
        (throw
         (ex-info
          "Method not allowed"
          {::response
           {:status 405
            :headers {"allow" (allow-header methods)}
            :body "Method Not Allowed\r\n"}}))))
    ;; We forbid POST, PUT and DELETE on a nil resource
    (when (#{:put :delete :post} (:request-method request))
      {:status 405
       :headers {"allow" (allow-header #{:get :head})}
       :body "Method Not Allowed\r\n"})))

(defn check-current-representations! [current-representations]
  (when (empty? current-representations)
    (throw
     (ex-info
      "Not Found"
      {::response
       {:status 404
        :body "Not Found\r\n"}}))))

(defn check-acceptable! [selected-representation]
  (when-not selected-representation
    (throw
     (ex-info
      "Not Acceptable"
      { ;; TODO: Must add list of available representations
       ::response
       {:status 406
        :body "Not Acceptable\r\n"}}))))

(defn head? [request]
  (= (:request-method request) :head))

(defn bad-request []
  {:status 400
   :body "Bad Request\r\n"})

(defn created
  "Convenience function for returning a 201 repsonse with a Location header."
  [location]
  {:status 201
   :headers {"location" location}
   :body "Created\r\n"})

(defn authenticate-challenge
  "Convenience function for returning a 201 repsonse with a Location header."
  [challenge]
  {:status 401
   :headers {"www-authenticate" challenge}
   :body "Unauthorized\r\n"})

(defn options
  [methods]
  {:status 200
   :headers
   {"allow" (allow-header methods)
    ;; TODO: Shouldn't this be a situation (a missing body) detected by
    ;; middleware, which can set the content-length header accordingly?
    "content-length" "0"}})

(defn evaluate-if-match!
  "Evaluate an If-None-Match precondition header field in the context of a
  resource. If the precondition is found to be false, an exception is thrown
  with ex-data containing the proper response."
  [request resource representation-metadata]
  ;; (All quotes in this function's comments are from Section 3.2, RFC 7232,
  ;; unless otherwise stated).
  (let [header-field (reap/if-match (get-in request [:headers "if-match"]))]
    (cond
      ;; "If the field-value is '*' …"
      (and (map? header-field) (::rfc7232/wildcard header-field))
      ;; "… the condition is false if the origin server does not have a current
      ;; representation for the target resource."
      (when (empty? (::representations resource))
        (throw
         (ex-info
          "If-Match precondition failed"
          {::message "No current representations for resource, so * doesn't match"
           ::response {:status 412
                       :body "Precondition Failed\r\n"}})))

      (sequential? header-field)
      (when-let [rep-etag (some-> (get representation-metadata "etag") reap/entity-tag)]
        (when-not (seq
                   (for [etag header-field
                         ;; "An origin server MUST use the strong comparison function
                         ;; when comparing entity-tags"
                         :when (rfc7232/strong-compare-match? etag rep-etag)]
                     etag))
          (throw
           (ex-info
            "If-Match precondition failed"
            {::message "No strong matches between if-match and current representations"
             ::if-match header-field
             ::response
             ;; TODO: "unless it can be determined that the state-changing
             ;; request has already succeeded (see Section 3.1)"
             {:status 412
              :body "Precondition Failed\r\n"}})))))))

(defn evaluate-if-none-match!
  "Evaluate an If-None-Match precondition header field in the context of a
  resource and, when applicable, the representation metadata of the selected
  representation. If the precondition is found to be false, an exception is
  thrown with ex-data containing the proper response."
  [request resource representation-metadata]
  ;; (All quotes in this function's comments are from Section 3.2, RFC 7232,
  ;; unless otherwise stated).
  (let [header-field (reap/if-none-match (get-in request [:headers "if-none-match"]))]
    (cond
      (sequential? header-field)
      (when-let [rep-etag (some-> (get representation-metadata "etag") reap/entity-tag)]
        ;; "If the field-value is a list of entity-tags, the condition is false
        ;; if one of the listed tags match the entity-tag of the selected
        ;; representation."
        (doseq [etag header-field]
          ;; "A recipient MUST use the weak comparison function when comparing
          ;; entity-tags …"
          (when (rfc7232/weak-compare-match? etag rep-etag)
            (throw
             (ex-info
              "If-None-Match precondition failed"
              {::message "One of the etags in the if-none-match header matches the selected representation"
               ::entity-tag etag
               ::representation-metadata representation-metadata
               ::response
               ;; "the origin server MUST respond with either a) the 304 (Not
               ;; Modified) status code if the request method is GET or HEAD …"
               (if (#{:get :head} (:request-method request))
                 {:status 304
                  :body "Not Modified\r\n"}
                 ;; "… or 412 (Precondition Failed) status code for all other
                 ;; request methods."
                 {:status 304
                  :body "Precondition Failed\r\n"})})))))

      ;; "If-None-Match can also be used with a value of '*' …"
      (and (map? header-field) (::rfc7232/wildcard header-field))
      ;; "… the condition is false if the origin server has a current
      ;; representation for the target resource."
      (when (seq (::representations resource))
        (throw
         (ex-info
          "If-None-Match precondition failed"
          {::message "At least one representation already exists for this resource"
           ::resource resource
           ::response
           ;; "the origin server MUST respond with either a) the 304 (Not
           ;; Modified) status code if the request method is GET or HEAD …"
           (if (#{:get :head} (:request-method request))
             {:status 304
              :body "Not Modified\r\n"}
             ;; "… or 412 (Precondition Failed) status code for all other
             ;; request methods."
             {:status 412
              :body "Precondition Failed\r\n"})}))))))

(defn evaluate-if-unmodified-since! [if-unmodified-since representation-metadata]
  (let [if-unmodified-since-date (::rfc7231/date (reap/http-date if-unmodified-since))
        rep-last-modified-date (some-> (get representation-metadata "last-modified") reap/http-date ::rfc7231/date)]
    (when (.isAfter
           (.toInstant rep-last-modified-date)
           (.toInstant if-unmodified-since-date))
      (throw
       (ex-info
        "Precondition failed"
        {::representation-metadata representation-metadata
         ::response
         {:status 412 :body "Precondition Failed\r\n"}})))))

(defn evaluate-if-modified-since! [if-modified-since representation-metadata]
  (let [if-modified-since-date (::rfc7231/date (reap/http-date if-modified-since))
        rep-last-modified-date (some-> (get representation-metadata "last-modified") reap/http-date ::rfc7231/date)]
    (assert if-modified-since-date)
    (assert rep-last-modified-date)

    (when-not (.isAfter
               (.toInstant rep-last-modified-date)
               (.toInstant if-modified-since-date))
      (throw
       (ex-info
        "Not modified"
        {::representation-metadata representation-metadata
         ::response
         {:status 304 :body "Not Modified\r\n"}})))))

(defn evaluate-preconditions!
  "Implementation of Section 6 of RFC 7232."
  [request resource representation-metadata]
  ;; "… a server MUST ignore the conditional request header fields … when
  ;; received with a request method that does not involve the selection or
  ;; modification of a selected representation, such as CONNECT, OPTIONS, or
  ;; TRACE." -- Section 5, RFC 7232
  (when (not (#{:connect :options :trace} (:request-method request)))
    (if (get-in request [:headers "if-match"])
      ;; Step 1
      (evaluate-if-match! request resource representation-metadata)
      ;; Step 2
      (when-let [if-unmodified-since (get-in request [:headers "if-match"])]
        (evaluate-if-unmodified-since! if-unmodified-since representation-metadata)))
    ;; Step 3
    (if (get-in request [:headers "if-none-match"])
      (evaluate-if-none-match! request resource representation-metadata)
      ;; Step 4, else branch: if-none-match is not present
      (when (#{:get :head} (:request-method request))
        (when-let [if-modified-since (get-in request [:headers "if-modified-since"])]
          (evaluate-if-modified-since! if-modified-since representation-metadata))))

    ;; Step 5 (range requests)
    ;; (TODO)

    ;; Step 6: continue
    ;; TODO: We should attempt to run this in a transaction when modifying the resource
    ))

(defn request-range [request
                     {::keys [accept-ranges] :as resource}
                     selected-representation-metadata]
  (assert resource)
  (when-let [range-header-value (get-in request [:headers "range"])]
    (let [parsed-range
          (try
            (or
             (reap/range range-header-value)
             (throw (ex-info "Parsing header yields nil" {})))
            (catch clojure.lang.ExceptionInfo e
              (throw
               (ex-info
                "Bad Range header value"
                {::range range-header-value
                 ::response {:status 400
                             :body "Bad Range header value\r\n"}}
                e))))]

      ;; "An origin server MUST ignore a Range header field that contains a
      ;; range unit it does not understand." -- Section 3.1, RFC 7233
      (when (and accept-ranges
                 (contains?
                  (set accept-ranges)
                  (:juxt.reap.alpha.rfc7233/units parsed-range)))

        (if-let [if-range-header-value (get-in request [:headers "if-range"])]
          (let [parsed-if-range (reap/if-range if-range-header-value)]
            (when
                (or
                 (and
                  (::rfc7232/entity-tag parsed-if-range)
                  (rfc7232/strong-compare-match?
                   (::rfc7232/entity-tag parsed-if-range)
                   (some-> selected-representation-metadata (get "etag") reap/entity-tag)))

                 (and
                  (::rfc7231/http-date parsed-if-range)
                  (= (::rfc7231/date (::rfc7231/http-date parsed-if-range))
                     (some-> selected-representation-metadata
                             (get "last-modified")
                             reap/http-date
                             ::rfc7231/date))))
              parsed-range))

          ;; No If-Range header.
          parsed-range)))))
