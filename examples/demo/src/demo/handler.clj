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
   [juxt.reap.alpha.ring :refer [headers->decoded-preferences]]
   [juxt.reap.alpha.encoders :refer [format-http-date]]
   [juxt.spin.alpha :as spin]))

(defn GET [request resource
           date selected-representation selected-representation-metadata
           current-representations vary
           opts]

  (when (empty? current-representations)
    (throw
     (ex-info
      "Not Found"
      {::spin/response
       {:status 404
        :body "Not Found\r\n"}})))

  (when-not selected-representation
    (throw
     (ex-info
      "Not Acceptable"
      { ;; TODO: Must add list of available representations
       ::spin/response
       {:status 406
        :body "Not Acceptable\r\n"}})))

  (spin/evaluate-preconditions! request resource selected-representation-metadata)

  ;; "The Range header field is evaluated after evaluating the precondition
  ;; header fields defined in [RFC7232], and only if the result in absence
  ;; of the Range header field would be a 200 (OK) response.  In other
  ;; words, Range is ignored when a conditional GET would result in a 304
  ;; (Not Modified) response.

  (let [ranges-specifier (spin/request-range request resource selected-representation-metadata)

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

(defn POST [request resource date {::spin/keys [db-atom]}]

  (assert (= (:uri request) (::spin/path resource)))

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
                (assoc ::spin/representations [new-representation])
                (dissoc ::spin/path))]

        (swap!
         db-atom
         (fn [db]
           (spin/evaluate-preconditions! request resource selected-representation-metadata)
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
                    (spin/evaluate-preconditions! request resource selected-representation-metadata)
                    (update % :resources dissoc (::spin/path resource))))
  (cond-> {:status 200}
    date (assoc "date" (format-http-date date))
    true (assoc :body "Deleted\r\n")))

(defn OPTIONS [_ resource _ _]
  ;; TODO: Implement *
  (spin/options (::spin/methods resource)))

(defn make-handler [*database]
  (fn [request]
    (let [db @*database]
      (try
        ;; Check method implemented
        (spin/check-method-not-implemented! request)

        ;; Locate the resource
        (let [resource (app/locate-resource db (:uri request))]

          ;; Check method allowed
          (spin/check-method-not-allowed! request resource)

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
;;          (tap> e)
          (let [exdata (ex-data e)]
            (or
             (::spin/response exdata)
             {:status 500 :body "Internal Error\r\n"})))))))