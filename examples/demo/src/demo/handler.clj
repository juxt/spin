;; Copyright © 2020, JUXT LTD.

(ns demo.handler
  (:require
   [demo.app :as app]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [demo.representation
    :refer [make-byte-array-representation
            make-char-sequence-representation
            representation-metadata
            payload]]
   [juxt.pick.alpha.core :refer [rate-representation]]
   [juxt.pick.alpha.ring :refer [pick decode-maybe]]
   [juxt.reap.alpha.decoders :as reap]
   [juxt.reap.alpha.ring :refer [headers->decoded-preferences]]
   [juxt.reap.alpha.encoders :refer [format-http-date]]
   [juxt.reap.alpha.rfc7231 :as rfc7231]
   [juxt.reap.alpha.rfc7232 :as rfc7232]
   [juxt.reap.alpha.rfc7233 :as rfc7233]
   [juxt.spin.alpha :as spin]))

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
             {:status 304
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
                     {:demo.app/keys [accept-ranges] :as resource}
                     selected-representation-metadata]
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
                {:range range-header-value
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

(defn GET [request {:demo.app/keys [accept-ranges] :as resource}
           date selected-representation selected-representation-metadata
           current-representations vary
           {:demo.app/keys [db]}]

  (when (empty? current-representations)
    (throw
     (ex-info
      "Not Found"
      {::response
       {:status 404
        :body "Not Found\r\n"}})))

  (when-not selected-representation
    (throw
     (ex-info
      "Not Acceptable"
      { ;; TODO: Must add list of available representations
       ::response
       {:status 406
        :body "Not Acceptable\r\n"}})))

  (evaluate-preconditions! request resource selected-representation-metadata)

  ;; "The Range header field is evaluated after evaluating the precondition
  ;; header fields defined in [RFC7232], and only if the result in absence
  ;; of the Range header field would be a 200 (OK) response.  In other
  ;; words, Range is ignored when a conditional GET would result in a 304
  ;; (Not Modified) response.

  (let [ranges-specifier (request-range request resource selected-representation-metadata)

        {status :status
         payload-headers :headers
         body :body}
        (payload selected-representation
                 selected-representation-metadata
                 date
                 ranges-specifier
                 {:demo.app/db db})]

    (cond-> {:status (or status 200)
             :headers
             (cond-> {}
               date (assoc "date" (format-http-date date))

               (:demo.app/accept-ranges resource)
               (assoc "accept-ranges" (str/join ", " (:demo.app/accept-ranges resource)))

               (seq vary) (assoc "vary" (str/join ", " vary))

               selected-representation-metadata (merge selected-representation-metadata)

               (= (get selected-representation-metadata "content-location") (:uri request))
               (dissoc "content-location")

               payload-headers (merge payload-headers))}

      ;; Don't add body for a HEAD method
      (= (:request-method request) :get) (assoc :body body))))

(defn POST [request resource date {:demo.app/keys [db-atom]}]

  (assert (= (:uri request) (:demo.app/path resource)))

  ;; Revisit - we should not encode knowledge of the URI structure here.
  (case (:uri request)
    ;; TODO: Should look at some hint in the resource as to functionality,
    ;; perhaps via a defmethod
    "/comments"
    (do
      (when-not (get-in request [:headers "content-length"])
        (throw
         (ex-info
          "No Content-Length header found"
          {::response
           {:status 411
            :body "Length Required\r\n"}})))

      (when-not (:body request)
        (throw
         (ex-info
          "No body in request"
          {::response
           {:status 400
            :body "Bad Request\r\n"}})))

      ;; TODO: Check input type, can throw a 415 error response if necessary
      ;; (But use reap)
      ;; (if (= (get-in request [:headers "content-type"]) "text/plain"))

      (let [out (java.io.ByteArrayOutputStream.)
            _ (with-open [in (:body request)]
                (io/copy in out))
            new-db (swap!
                    db-atom
                    (fn [db]
                      ;; TODO: Any preconditions?
                      (app/add-comment db (String. (.toByteArray out)))))]
        (cond->
            (spin/created (:last-location new-db))
          date (assoc "date" (format-http-date date))
          ;; TODO: Add optional payload, with payload headers
          )))))

(defn PUT
  "Replace the state of a resource with the state defined by the representation
  enclosed in the request message payload. Neither argument can be nil."
  [request resource selected-representation-metadata date {:demo.app/keys [db-atom]}]

  (assert (= (:uri request) (:demo.app/path resource)))

  ;; If resource just has one representation, we wish to put over it. We should
  ;; be able to do this in the general case.

  (when-not (get-in request [:headers "content-length"])
    (throw
     (ex-info
      "No Content-Length header found"
      {::response
       {:status 411
        :body "Length Required\r\n"}})))

  (if-let [content-length (get-in request [:headers "content-length"])]
    (when-let [max-content-length (:demo.app/max-content-length resource)]
      (try
        (let [content-length (Long/parseLong content-length)]
          (when (> content-length max-content-length)
            (throw
             (ex-info
              "Payload too large"
              {::response
               {:status 413
                :body "Payload Too Large\r\n"}}))))

        (catch NumberFormatException e
          (throw
           (ex-info
            "Bad content length"
            {::response
             {:status 400
              :body "Bad Request\r\n"}}
            e)))))

    ;; No content-length
    (throw
     (ex-info
      "Length Required"
      {::response
       {:status 411
        :body "Length Required\r\n"}})))

  (when-not (:body request)
    (throw
     (ex-info
      "No body in request"
      {::response
       {:status 400
        :body "Bad Request\r\n"}})))

  (let [decoded-representation
        (decode-maybe
         (select-keys
          (merge {"content-encoding" "identity"} (:headers request))
          ["content-type"
           "content-encoding"
           "content-language"]))]

    (when-let [acceptable (:demo.app/acceptable resource)]
      (let [prefs (headers->decoded-preferences acceptable)
            request-rep (rate-representation prefs decoded-representation)]

        (when (or (get prefs "accept") (get prefs "accept-charset"))
          (cond
            (not (contains? (:headers request) "content-type"))
            (throw
             (ex-info
              "Request must contain Content-Type header"
              {::response
               {:status 415
                :body "Unsupported Media Type\r\n"}}))

            (= (:juxt.pick.alpha/content-type-qvalue request-rep) 0.0)
            (throw
             (ex-info
              "The content-type of the request payload is not supported by the resource"
              {::request request
               ::resource resource
               ::acceptable acceptable
               ::content-type (get request-rep "content-type")
               ::response
               {:status 415
                :body "Unsupported Media Type\r\n"}}))

            (and
             (= "text" (get-in request-rep [:juxt.reap.alpha.rfc7231/content-type :juxt.reap.alpha.rfc7231/type]))
             (get prefs "accept-charset")
             (not (contains? (get-in request-rep [:juxt.reap.alpha.rfc7231/content-type :juxt.reap.alpha.rfc7231/parameter-map]) "charset")))
            (throw
             (ex-info
              "The Content-Type header in the request is a text type and is required to specify its charset as a media-type parameter"
              {::request request
               ::resource resource
               ::acceptable acceptable
               ::content-type (get request-rep "content-type")
               ::response
               {:status 415
                :body "Unsupported Media Type\r\n"}}))

            (= (:juxt.pick.alpha/charset-qvalue request-rep) 0.0)
            (throw
             (ex-info
              "The charset of the Content-Type header in the request is not supported by the resource"
              {::request request
               ::resource resource
               ::acceptable acceptable
               ::content-type (get request-rep "content-type")
               ::response
               {:status 415
                :body "Unsupported Media Type\r\n"}}))))

        (when (get prefs "accept-encoding")
          (cond
            (= (:juxt.pick.alpha/content-encoding-qvalue request-rep) 0.0)
            (throw
             (ex-info
              "The content-encoding in the request is not supported by the resource"
              {::request request
               ::resource resource
               ::acceptable acceptable
               ::content-language (get-in request [:headers "content-encoding"] "identity")
               ::response
               {:status 409
                :body "Conflict\r\n"}}))))

        (when (get prefs "accept-language")
          (cond
            (not (contains? (:headers request) "content-language"))
            (throw
             (ex-info
              "Request must contain Content-Language header"
              {::response
               {:status 409
                :body "Conflict\r\n"}}))

            (= (:juxt.pick.alpha/content-language-qvalue request-rep) 0.0)
            (throw
             (ex-info
              "The content-language in the request is not supported by the resource"
              {::request request
               ::resource resource
               ::acceptable acceptable
               ::content-language (get-in request [:headers "content-language"])
               ::response
               {:status 415
                :body "Unsupported Media Type\r\n"}}))))))

    (let [out (java.io.ByteArrayOutputStream.)]
      (with-open [in (:body request)]
        (io/copy in out))

      (let [content-type (:juxt.reap.alpha.rfc7231/content-type decoded-representation)
            charset (get-in decoded-representation [:juxt.reap.alpha.rfc7231/content-type :juxt.reap.alpha.rfc7231/parameter-map "charset"])
            new-representation-metadata
            (merge
             (select-keys
              (:headers request)
              ["content-type" "content-language" "content-encoding"])
             {"last-modified" (format-http-date (new java.util.Date))})

            new-representation
            (cond
              (= (:juxt.reap.alpha.rfc7231/type content-type) "text")
              (make-char-sequence-representation
               (new String (.toByteArray out) charset)
               new-representation-metadata)

              :else
              (make-byte-array-representation
               (.toByteArray out)
               new-representation-metadata))

            new-resource
            (-> resource
                (assoc :demo.app/representations [new-representation])
                (dissoc :demo.app/path))]

        (swap!
         db-atom
         (fn [db]
           (evaluate-preconditions! request resource selected-representation-metadata)
           (assoc-in
            db [:resources (:uri request)] new-resource)))

        ;; TODO: Must read 6.3.2 and 7.2 to properly understand 201, especially: "The
        ;; 201 response payload typically describes and links to the resource(s)
        ;; created."

        ;; "If the target resource does not have a current representation and the PUT
        ;; successfully creates one, then the origin server MUST inform the user agent
        ;; by sending a 201 (Created) response."

        (cond-> {:status 200}
          date (assoc "date" (format-http-date date)))))))

(defn DELETE [request resource selected-representation-metadata date {:demo.app/keys [db-atom]}]
  (swap! db-atom #(do
                    (evaluate-preconditions! request resource selected-representation-metadata)
                    (update % :resources dissoc (:demo.app/path resource))))
  (cond-> {:status 200}
    date (assoc "date" (format-http-date date))
    true (assoc :body "Deleted\r\n")))

(defn OPTIONS [_ resource _ _]
  ;; TODO: Implement *
  (spin/options (:demo.app/methods resource)))

(defn make-handler [*database]
  (fn [request]
    (let [db @*database]
      (try
        ;; Check method implemented
        (when-let [response (spin/not-implemented? request)]
          (throw (ex-info "Method not implemented" {::response response})))

        ;; Locate the resource
        (let [resource (app/locate-resource db (:uri request))]

          ;; Check method allowed
          (when-let [response
                     (if resource
                       (spin/method-not-allowed? request (:demo.app/methods resource))
                       ;; We forbid POST, PUT and DELETE on a nil resource
                       (when (#{:put :delete :post} (:request-method request))
                         {:status 405
                          :headers {"allow" (spin/allow-header #{:get :head})}
                          :body "Method Not Allowed\r\n"}))]
            (throw (ex-info "Method not allowed" {::response response})))

          (let [ ;; Fix the date, this will be used as the message origination
                 ;; date.
                date (new java.util.Date)

                ;; Get the 'current' representations of the resource.
                current-representations (app/current-representations db resource)

                opts {:demo.app/db-atom *database}

                ;; Negotiate the best representation, determining the vary
                ;; header.
                {representation-metadata :juxt.pick.alpha/representation
                 vary :juxt.pick.alpha/vary}
                (when (seq current-representations)
                  ;; Call into pick which does that actual content-negotation
                  ;; for us.
                  (pick
                   request
                   (for [r current-representations]
                     (-> r
                         (representation-metadata date opts)
                         (assoc ::attached-representation r)))
                   {:juxt.pick.alpha/vary? true}))

                selected-representation (::attached-representation representation-metadata)

                representation-metadata
                (as-> representation-metadata %
                  (dissoc % ::attached-representation)
                  ;; Remove the extraneous keyword entries added by pick.
                  (filter (comp string? first) %)
                  (into {} %))]

            ;; Process the request method
            (case (:request-method request)
              (:get :head)
              (GET request resource
                   date selected-representation representation-metadata
                   current-representations vary
                   {:demo.app/db db})

              :post
              (POST request resource date opts)

              :put
              (PUT request resource representation-metadata date opts)

              :delete
              (DELETE request resource representation-metadata date opts)

              :options
              (OPTIONS request resource date opts))))

        (catch clojure.lang.ExceptionInfo e
          ;;(tap> e)
          (let [exdata (ex-data e)]
            (or
             (::response exdata)
             {:status 500 :body "Internal Error\r\n"})))))))
