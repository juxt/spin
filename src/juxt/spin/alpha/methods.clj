;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.methods
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [juxt.pick.alpha.core :refer [rate-representation]]
   [juxt.pick.alpha.ring :refer [decode-maybe]]
   [juxt.reap.alpha.encoders :refer [format-http-date]]
   [juxt.reap.alpha.ring :refer [headers->decoded-preferences]]
   [juxt.spin.alpha :as spin]
   [juxt.spin.alpha.representation :refer [make-byte-array-representation
                                           make-char-sequence-representation
                                           payload]]))

(defn GET
  "The GET method."
  [request resource
   date selected-representation selected-representation-metadata
   current-representations vary
   opts]

  ;; Check for a 404 Not Found
  (spin/check-not-found! current-representations)

  ;; Check for a 406 Not Acceptable
  (spin/check-not-acceptable! selected-representation)

  ;; Check for a 304 Not Modified
  (spin/evaluate-preconditions! request resource selected-representation-metadata)

  ;; "The Range header field is evaluated after evaluating the precondition
  ;; header fields defined in [RFC7232], and only if the result in absence
  ;; of the Range header field would be a 200 (OK) response.  In other
  ;; words, Range is ignored when a conditional GET would result in a 304
  ;; (Not Modified) response.

  (let [ranges-specifier (spin/request-range request resource selected-representation-metadata)

        ;; Here we determine the status (optional), payload headers and body of
        ;; the representation.
        {status :status
         payload-headers :headers
         body :body}
        (payload selected-representation
                 selected-representation-metadata
                 date
                 ranges-specifier
                 opts)]

    (cond-> {:status (or status 200)
             :headers
             (cond-> {}
               date (assoc "date" (format-http-date date))

               (::spin/accept-ranges resource)
               (assoc "accept-ranges" (str/join ", " (::spin/accept-ranges resource)))

               (seq vary) (assoc "vary" (str/join ", " vary))

               selected-representation-metadata (merge selected-representation-metadata)

               (= (get selected-representation-metadata "content-location") (:uri request))
               (dissoc "content-location")

               payload-headers (merge payload-headers))}

      ;; Don't add body for a HEAD method
      (= (:request-method request) :get) (assoc :body body))))


(defn extract-representation-from-request
  "Replace the state of a resource with the state defined by the representation
  enclosed in the request message payload. Neither argument can be nil."
  [request resource selected-representation-metadata date]

  (assert (= (:uri request) (::spin/path resource)))

  ;; If resource just has one representation, we wish to put over it. We should
  ;; be able to do this in the general case.

  (when-not (get-in request [:headers "content-length"])
    (throw
     (ex-info
      "No Content-Length header found"
      {::spin/response
       {:status 411
        :body "Length Required\r\n"}})))

  (if-let [content-length (get-in request [:headers "content-length"])]
    (when-let [max-content-length (::spin/max-content-length resource)]
      (try
        (let [content-length (Long/parseLong content-length)]
          (when (> content-length max-content-length)
            (throw
             (ex-info
              "Payload too large"
              {::spin/response
               {:status 413
                :body "Payload Too Large\r\n"}}))))

        (catch NumberFormatException e
          (throw
           (ex-info
            "Bad content length"
            {::spin/response
             {:status 400
              :body "Bad Request\r\n"}}
            e)))))

    ;; No content-length
    (throw
     (ex-info
      "Length Required"
      {::spin/response
       {:status 411
        :body "Length Required\r\n"}})))

  (when-not (:body request)
    (throw
     (ex-info
      "No body in request"
      {::spin/response
       {:status 400
        :body "Bad Request\r\n"}})))

  (let [decoded-representation
        (decode-maybe
         (select-keys
          (merge {"content-encoding" "identity"} (:headers request))
          ["content-type"
           "content-encoding"
           "content-language"]))]

    (when-let [acceptable (::spin/acceptable resource)]
      (let [prefs (headers->decoded-preferences acceptable)
            request-rep (rate-representation prefs decoded-representation)]

        (when (or (get prefs "accept") (get prefs "accept-charset"))
          (cond
            (not (contains? (:headers request) "content-type"))
            (throw
             (ex-info
              "Request must contain Content-Type header"
              {::spin/response
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
               ::spin/response
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
               ::spin/response
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
               ::spin/response
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
               ::spin/response
               {:status 409
                :body "Conflict\r\n"}}))))

        (when (get prefs "accept-language")
          (cond
            (not (contains? (:headers request) "content-language"))
            (throw
             (ex-info
              "Request must contain Content-Language header"
              {::spin/response
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
               ::spin/response
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
             {"last-modified" (format-http-date (new java.util.Date))})]

        (cond
          (= (:juxt.reap.alpha.rfc7231/type content-type) "text")
          (make-char-sequence-representation
           (new String (.toByteArray out) charset)
           new-representation-metadata)

          :else
          (make-byte-array-representation
           (.toByteArray out)
           new-representation-metadata))))))
